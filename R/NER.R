#' Analyse a sentence, document or corpus
#'
#' Extract SNOMED CT concepts from the text, using the composition
#' method to find the correct, specific SNOMED code.
#' These functions do not apply any disambiguation or context detection.
#'
#' @param text a single text to analyse
#' @param texts a character vector of texts to analyse
#' @param ids vector of IDs for texts to analyse
#' @param SNOMED environment containing a SNOMED dictionary
#' @param CDB environment containing a concept database, to which
#'   the composition lookup has been added using addComposeLookupToCDB
#'   and terms have been lemmatized using addLemmaToCDB
#' @param noisy TRUE or FALSE, whether to display intermediate results
#' @seealso addLemmaToCDB
#' @return NERsentence and NERdocument returns a data.table containing
#'   spacy parse variables: token (the token itself), lemma (spacy
#'   lemmatized version of token), dep_rel (dependency relation),
#'   head_token_id (token ID of dependency link), semType (semantic
#'   type if mapped to a SNOMED CT concept), token_id. In the case of
#'   NERdocument, there is an additional column named 'sentence
#'   containing the sentence number. There is an attribute
#'   'annotations' which is a data.table containing the annotated
#'   SNOMED CT concepts, and an attribute 'findings' which is a
#'   data.table with columns conceptId and term, containing the
#'   set of distinct finding or disorder concepts extracted. 
#'   NERcorpus returns a data.table with columns id (text document
#'   identifier), conceptId and term.
#' @examples
#' data.table::setDTthreads(threads = 1)
#'
#' # Create a mini CDB based on the sample SNOMED dictionary
#' miniSNOMED <- sampleSNOMED()
#' miniCDB <- createCDB(SNOMED = miniSNOMED)
#'
#' # 83291003 | Cor pulmonale (disorder) 
#' # 19829001 | Disorder of lung (disorder)
#' # Decompose 2 concepts and add the decomposition lookup to CDB
#' DEC <- decompose(SNOMEDconcept(c('83291003', '19829001'),
#'   SNOMED = miniSNOMED), CDB = miniCDB, SNOMED = miniSNOMED)
#' miniCDB <- addComposeLookupToCDB(DEC, CDB = miniCDB)
#' miniCDB <- addLemmaToCDB(miniCDB)
#'
#' # NER on a simple sentence
#' NERsentence('Cardiomyopathy', CDB = miniCDB, SNOMED = miniSNOMED)
#' 
#' # Parsed text:
#' #             token          lemma dep_rel head_token_id  semType
#' #            <char>         <char>  <char>         <num>   <char>
#' # 1: Cardiomyopathy Cardiomyopathy    ROOT             1 disorder
#' # 
#' # Before refinement:
#' #          conceptId startword endword startwhole endwhole  semType due_to
#' #    <SNOMEDconcept>     <num>   <num>      <num>    <num>   <char> <list>
#' # 1:        85898001         1       1          1        1 disorder [NULL]
#' #    causing attributes link_to      laterality                      term
#' #     <list>     <list>   <int> <SNOMEDconcept>                    <char>
#' # 1:  [NULL]     [NULL]      NA            <NA> Cardiomyopathy (disorder)
#' # 
#' # After refinement:
#' #          conceptId                      term  words
#' #    <SNOMEDconcept>                    <char> <char>
#' # 1:        85898001 Cardiomyopathy (disorder)    1-1
#' # 
#' # Remove ancestors of other concepts and
#' # single word overlapped concepts in final output:
#' #          conceptId                      term  words
#' #    <SNOMEDconcept>                    <char> <char>
#' # 1:        85898001 Cardiomyopathy (disorder)    1-1
#' #          conceptId                      term
#' #    <SNOMEDconcept>                    <char>
#' # 1:        85898001 Cardiomyopathy (disorder)
#' # 
#' # Test NER sentence with SNOMED concept composition
#' attr(NERsentence('Right heart failure caused by lung disease',
#'   CDB = miniCDB, SNOMED = miniSNOMED, noisy = FALSE), 'findings')
#'
#' # Test NER document
#' attr(NERdocument('Cardiomyopathy. 
#'   Right heart failure caused by lung disease',
#'   CDB = miniCDB, SNOMED = miniSNOMED), 'findings')
#'  
#' # Test NER corpus
#' NERcorpus(c('No concepts here', 'Cardiomyopathy. 
#'   Right heart failure caused by lung disease'), 1:2,
#'   CDB = miniCDB, SNOMED = miniSNOMED)
#' 
#' @rdname NERsentence
#' @export
NERsentence <- function(text, CDB, SNOMED, noisy = TRUE){
	# Apply the spacy parser and do lookup for SNOMED CT concepts
	D <- parseSentence(text, CDB, SNOMED)
	
	# Add laterality and body site links
	D <- addLateralityBodyLinks(D, CDB, SNOMED)
	
	# Add other attributes based on dependency parser
	D <- addAttributeLinks(D, CDB, SNOMED)
	
	# Add attributes based on proximity
	D <- addProximityLinks(D, CDB, SNOMED)
	
	if (noisy){
		cat('\nParsed text:\n')
		print(D)
		cat('\nBefore refinement:\n')
		print(attr(D, 'annotations'))
		cat('\nAfter refinement:\n')
	}
	
	showannotations <- function(D){
		C <- attr(D, 'annotations')
		if (nrow(C) > 0){
			return(C[, list(startwhole = min(startwhole),
				endwhole = max(endwhole)),
				by = .(conceptId, term)][, .(conceptId, term,
				words = paste0(startwhole, '-', endwhole))])
		} else {
			return(data.table(conceptId = bit64::as.integer64(0),
				term = character(0), words = character(0)))
		}
	}
	
	# Use the composeLookup to refine the SNOMED CT concepts
	# including updating semantic types if qualifiers etc. are
	# incorporated into finding concepts
	D <- refineFindings(D, CDB, SNOMED)
	
	if (noisy) print(showannotations(D))
	
	# Add causal links if any
	D <- addCausalLinks(D, CDB, SNOMED)
	
	if (!all(sapply(attr(D, 'annotations')$due_to, is.null))){
		# If any causal links (due to) were added, refine findings again
		D <- refineFindings(D, CDB, SNOMED)
		if (noisy){
			cat('\nAfter refinement of cause:\n')
			print(showannotations(D))
		}
	}
	
	if (noisy){
		cat(paste0('\nRemove ancestors of other concepts and\n',
			'single word overlapped concepts in final output:\n'))
	}

	# Remove concepts that are ancestors of another concept (to tidy
	# the output and leave only the most specific concepts)
	D <- removeAncestorsD(D, CDB, SNOMED)
	
	# Remove single word overlapped findings
	D <- removeSingleWordOverlappedFindingsD(D, CDB, SNOMED)
	
	if (noisy) print(showannotations(D))
	
	D[, token_id := 1:.N]
	F <- attr(D, 'annotations')[
		semType %in% c('finding', 'disorder'), list(conceptId, term)]
	F <- F[!duplicated(F)]
	setattr(D, 'findings', F)
	if (noisy){
		print(attr(D, 'findings'))
	}
	invisible(D)
}

#' @rdname NERsentence
#' @export
NERdocument <- function(text, CDB, SNOMED){
	# returns 
	sentences <- strsplit(text, '\\\\n|\\. ')[[1]]
	D <- lapply(seq_along(sentences), function(i){
		x <- NERsentence(sentences[i], CDB, SNOMED,
			noisy = FALSE)
		x[, sentence := i]
		x
	})
	C <- rbindlist(lapply(seq_along(sentences), function(i){
		x <- copy(attr(D[[i]], 'annotations'))
		x[, sentence := i]
		x
	}))
	setattr(D, 'annotations', C)
	F <- C[semType %in% c('finding', 'disorder'),
		list(sentence, conceptId, term)]
	F <- F[!duplicated(F)]
	setattr(D, 'findings', F)
	D
}

#' @rdname NERsentence
#' @export
NERcorpus <- function(texts, ids = seq_along(texts),
	CDB, SNOMED){
	# returns findings
	if (length(ids) != length(texts)){
		stop('ids must the same length as texts')
	}
	D <- lapply(seq_along(ids), function(i){
		NERdocument(texts[i], CDB, SNOMED)
	})
	F <- rbindlist(lapply(seq_along(ids), function(i){
		if (nrow(attr(D[[i]], 'annotations')) == 0){
			data.table(id = ids[i],
				conceptId = SNOMEDconcept('', SNOMED = SNOMED)[0],
				term = character(0))
		} else {
			attr(D[[i]], 'annotations')[semType %in%
				c('finding', 'disorder')][,
				list(id = ids[i], conceptId, term)]
		}
	}))
	F[!duplicated(F)]
}

#### FUNCTIONS FOR SEMANTIC LINKING AND CONCEPT REFINEMENT ####

addToAttributes <- function(C, index_row, attr_rows){
	# Adds a concept to the attribute list in the annotation table
	# (called by addSemanticLinks and addProximityLinks)
	if (length(attr_rows) > 0){
		C[index_row, attributes := list(unique(c(
			C[attr_rows]$conceptId, attributes[[1]])))]
		C[index_row, startwhole := min(startwhole, C[attr_rows]$startword)]
		C[index_row, endwhole := max(endwhole, C[attr_rows]$endword)]
	}
	C
}

addLateralityBodyLinks <- function(D, CDB, SNOMED){
	# Add semantic links based on spacy parsing for
	# laterality and body site
	# Store the results in the annotations table
	
	C <- attr(D, 'annotations')
	if (nrow(C) == 0) return(D)
	
	find_C_rows <- function(attr_words){
		# returns the row number in the annotation table (C)
		# corresponding to a word location in the original text
		unlist(lapply(attr_words, function(x){
			C[, which(x >= startwhole & x <= endwhole)]
		}))
	}

	# Laterality
	for (i in 1:nrow(C)){
		if (C[i]$semType %in% c('body structure', 'finding', 'disorder')){
			# if body site has intrinsic laterality, record it
			intrinsic_laterality <- CDB$BODY_LATERALITY[
				conceptId %in% C[i]$conceptId]$laterality
			if (identical(intrinsic_laterality, 'Right')){
				C[i, laterality := CDB$latConcepts['Right']]
				# SNOMED concept 24028007
			} else if (identical(intrinsic_laterality, 'Left')){
				C[i, laterality := CDB$latConcepts['Left']]
				# SNOMED concept 7771000
			} else {
				attr_rows <- find_C_rows(
					findLaterality(D, C[i]$startword:C[i]$endword))
				lat_conceptId <- unique(C[attr_rows]$conceptId)
				# if there is a unique laterality stated
				if (length(lat_conceptId) == 1){
					C[attr_rows, link_to := i]
					C[i, laterality := lat_conceptId]
					C[i, startwhole := min(startwhole, C[attr_rows]$startword)]
					C[i, endwhole := max(endwhole, C[attr_rows]$endword)]
				}
			}
		}
	}

	# Body site and transfer laterality
	for (i in 1:nrow(C)){
		if (C[i]$semType %in% c('finding', 'disorder', 'morphologic abnormality')){
			attr_rows <- find_C_rows(findBody(D, C[i]$startword:C[i]$endword))
			if (length(attr_rows) > 0){
				C[attr_rows, link_to := i]
				C <- addToAttributes(C, i, attr_rows)
				# Transfer laterality to corresponding finding if finding
				# does not have a laterality
				new_lat_conceptId <- unique(C[attr_rows]$laterality)
				if (length(new_lat_conceptId) == 1){
					if (is.na(C[i]$laterality)){
						C[i, laterality := new_lat_conceptId]
					} else {
						if (!identical(C[i]$laterality, new_lat_conceptId)){
							# remove laterality as it is ambiguous
							C[i, laterality := bit64::as.integer64(NA)]
						}
					}
				}
			}
		}
	}

	setattr(D, 'annotations', C)
	return(D)
}

addAttributeLinks <- function(D, CDB, SNOMED){
	# Add semantic links based on spacy parsing
	# Store the results in the annotations table
	
	C <- attr(D, 'annotations')
	if (nrow(C) == 0) return(D)
	
	find_C_rows <- function(attr_words){
		# returns the row number in the annotation table (C)
		# corresponding to a word location in the original text
		unlist(lapply(attr_words, function(x){
			C[, which(x >= startwhole & x <= endwhole)]
		}))
	}

	# Qualifiers including severity and stage, and cause (not laterality)
	for (i in 1:nrow(C)){
		if (C[i]$semType %in% c('finding', 'disorder',
			'morphologic abnormality')){
			attr_rows <- find_C_rows(c(
				findAttr(D, C[i]$startword:C[i]$endword),
				findCause(D, C[i]$startword:C[i]$endword)))
			if (length(attr_rows) > 0){
				C[attr_rows, link_to := i]
				C <- addToAttributes(C, i, attr_rows)
			}
		}
	}
	
	# Allergy / intolerance / adverse rection: link to substance
	for (i in 1:nrow(C)){
		if (C[i]$conceptId %in% CDB$allergyConcepts){
			attr_rows <- find_C_rows(
				findAllergy(D, C[i]$startword:C[i]$endword))
			if (length(attr_rows) > 0){
				C[attr_rows, link_to := i]
				C <- addToAttributes(C, i, attr_rows)
			}
		}
	}

	setattr(D, 'annotations', C)
	return(D)
}

addProximityLinks <- function(D, CDB, SNOMED, max_prox = 3){
	# Add links based on contiguous concepts before and after
	# finding for attributes other than laterality.
	# Stop on reaching a concept linked to another finding
	# or beyond the maximum number, set to 3 by default.

	C <- attr(D, 'annotations')
	if (nrow(C) == 0) return(D)
	
	root_types <- c('finding', 'disorder', 'morphologic abnormality')
	# Looping backwards through findings
	for (i in nrow(C):1){
		if (C[i]$semType %in% root_types){
			attr_rows <- integer(0)
			j <- i + 1
			end_run <- FALSE
			# Match forwards
			if (j < nrow(C)){
				while(length(attr_rows) < max_prox & j <= nrow(C) &
					!(C[j]$semType %in% root_types) & !end_run){
					if (C[j]$startword == C[j-1]$endword + 1 &
						C[j]$link_to %in% c(NA, i)){
						if (C[j]$semType != 'laterality'){
							C[j]$link_to <- i
							attr_rows <- c(attr_rows, j)
						}
						if (j < nrow(C)){
							j <- j + 1
						} else {
							end_run <- TRUE
						}
					} else {
						end_run <- TRUE
					}
				}
			}
			j <- i - 1
			end_run <- FALSE
			# Match backwards
			if (j > 0){
				while(length(attr_rows) < max_prox & j >= 1 &
					!(C[j]$semType %in% root_types) & !end_run){
					if (C[j]$endword == C[j+1]$startword - 1 &
						C[j]$link_to %in% c(NA, i)){
						if (C[j]$semType != 'laterality'){
							C[j]$link_to <- i
							attr_rows <- c(attr_rows, j)
						}
						if (j > 1){
							j <- j - 1
						} else {
							end_run <- TRUE
						}
					} else {
						end_run <- TRUE
					}
				}
			}
			C <- addToAttributes(C, i, attr_rows)
		}
	}
	
	setattr(D, 'annotations', C)
	return(D)
}

addCausalLinks <- function(D, CDB, SNOMED){
	# Add links for one finding/disease causing another based on 
	# findCause. Add the result to the due_to column of the
	# annotations table
	
	C <- attr(D, 'annotations')
	if (nrow(C) == 0) return(D)
	
	find_C_rows <- function(attr_words){
		unlist(lapply(attr_words, function(x){
			C[, which(x >= startword & x <= endword)]
		}))
	}
	
	root_types <- c('finding', 'disorder')

	# Combine multiple findings by causal links
	for (i in 1:nrow(C)){
		if (C[i]$semType %in% root_types){
			attr_rows <- intersect(
				which(C$semType %in% root_types),
				find_C_rows(findCause(D, C[i]$startword:C[i]$endword)))
			if (length(attr_rows) > 0){
				C[i, due_to := list(unique(
					c(C[attr_rows]$conceptId, due_to[[1]])))]
				C[i, startwhole := min(startwhole,
					C[attr_rows]$startwhole)]
				C[i, endwhole := max(endwhole,
					C[attr_rows]$endwhole)]
			}
		}
	}
	
	setattr(D, 'annotations', C)
	return(D)
}

removeAncestorsD <- function(D, CDB, SNOMED){
	# Remove concepts that are ancestors of another concept
	C <- attr(D, 'annotations')
	root_types <- c('finding', 'disorder', 'morphologic abnormality')
	matchIds <- as.SNOMEDconcept(unique(
		C[semType %in% root_types]$conceptId))
	matchIds <- remove_ancestors(matchIds)
	C[semType %in% root_types & !(conceptId %in% matchIds),
		semType := paste0('excl_a_', semType)]
	setattr(D, 'annotations', C)
	D
}

removeSingleWordOverlappedFindingsD <- function(D, CDB, SNOMED){
	# Remove single word findings that are overlapped by longer
	# concepts (which are more likely to be correct)
	C <- attr(D, 'annotations')
	multiword_rows <- C$endwhole > C$startwhole &
		C$semType %in% c('finding', 'disorder')
	singleword_rows <- C$endwhole == C$startwhole &
		C$semType %in% c('finding', 'disorder')
	if (sum(multiword_rows) > 0 & sum(singleword_rows) > 0){
		multiword_pos <- unique(unlist(lapply(which(multiword_rows),
			function(i){C[i]$startwhole:C[i]$endwhole})))
		C[singleword_rows & startword %in% multiword_pos, 
			semType := paste0('excl_s_', semType)]
	}
	setattr(D, 'annotations', C)
	D
}

remove_ancestors <- function(conceptIds){
	# Remove concepts that are ancestors of another concept from
	# a vector of concepts
	conceptIds <- as.SNOMEDconcept(conceptIds)
	if (length(conceptIds) == 0){
		# Remove all matches which are an ancestor of another match
		i <- 1
		while (i <= length(conceptIds)){
			ancIds <- ancestors(conceptIds[i], SNOMED = SNOMED,
				TRANSITIVE = CDB$TRANSITIVE, include_self = FALSE)
			if (length(intersect(matchIds, ancIds)) > 0){
				conceptIds <- setdiff(conceptIds, ancIds)
				i <- 1
			} else {
				i <- i + 1
			}
		}
	}
	conceptIds
}

refineFindings <- function(D, CDB, SNOMED){
	# Calls the 'compose' function which uses the composition lookup
	# to refine the SNOMED finding
	if (is.null(CDB$COMPOSELOOKUP)){
		# No composition performed 
		return(D)
	}
	
	C <- attr(D, 'annotations')
	EXTRA <- C[0]
	root_types <- c('finding', 'disorder', 'morphologic abnormality')
	finding_rows <- C[, which(semType %in% root_types)]
	for (i in finding_rows){
		refined_conceptId <- compose(C[i]$conceptId, CDB = CDB,
			attributes_conceptIds = c(C[i]$laterality, 
			C[i]$attributes[[1]]),
			due_to_conceptIds = C[i]$due_to[[1]],
			with_conceptIds = C[finding_rows]$conceptId,
			SNOMED = SNOMED)
		if (identical(C[i]$conceptId, refined_conceptId)){
			# Other attributes are irrelevant for this concept
			C[i, startwhole := startword]
			C[i, endwhole := endword] 
		} else {
			C[i, conceptId := refined_conceptId[1]]
			C[i, startword := startwhole]
			C[i, endword := endwhole]
			D[C[i]$startwhole:C[i]$endwhole, semType :=
				CDB$SEMTYPE[conceptId == C[i]$conceptId]$semType]
		}
		C[i, term := description(conceptId, SNOMED = SNOMED)$term]
		if (length(refined_conceptId) > 1) {
			TEMP <- C[rep(i, length(refined_conceptId) - 1)]
			TEMP[, conceptId :=
				refined_conceptId[2:length(refined_conceptId)]]
			TEMP[, term := description(conceptId, SNOMED = SNOMED)$term]
			EXTRA <- rbind(EXTRA, TEMP)
		}
	}
	C <- rbind(C, EXTRA)[order(startword)]
	setattr(D, 'annotations', C)
	D
}

