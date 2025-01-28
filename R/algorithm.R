
#### FUNCTIONS FOR SEMANTIC LINKING AND CONCEPT REFINEMENT ####

addToAttributes <- function(C, index_row, attr_rows){
	# Adds a concept to the attribute list in the annotation table
	# (called by addSemanticLinks and addProximityLinks)
	
	# Declare column names for CRAN check
	startwhole <- endwhole <- NULL
	
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
	
	# Declare column names for CRAN check
	startwhole <- endwhole <- conceptId <- laterality <- NULL
	link_to <- NULL
	
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
	
	# Declare column names for CRAN check
	startwhole <- endwhole <- NULL
	link_to <- NULL
	
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
	
	# Declare column names for CRAN check
	startwhole <- endwhole <- startword <- endword <- NULL
	due_to <- NULL
	
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
	
	# Declare column names for CRAN check
	semType <- conceptId <- NULL
	
	root_types <- c('finding', 'disorder')
	C <- attr(D, 'annotations')
	if (nrow(C) > 0){
		matchIds <- as.SNOMEDconcept(unique(
			C[semType %in% root_types]$conceptId))
		matchIds <- remove_ancestors(matchIds, CDB = CDB, 
			SNOMED = SNOMED)
		C[semType %in% root_types & !(conceptId %in% matchIds),
			semType := paste0('excl_a_', semType)]
		setattr(D, 'annotations', C)
	}
	D
}

removeSingleWordOverlappedFindingsD <- function(D){
	# Remove single word findings that are overlapped by longer
	# concepts (which are more likely to be correct)
	
	# Declare column names for CRAN check
	semType <- startword <- NULL
	
	root_types <- c('finding', 'disorder')
	C <- attr(D, 'annotations')
	multiword_rows <- C$endwhole > C$startwhole &
		C$semType %in% root_types
	singleword_rows <- C$endwhole == C$startwhole &
		C$semType %in% root_types
	if (sum(multiword_rows) > 0 & sum(singleword_rows) > 0){
		multiword_pos <- unique(unlist(lapply(which(multiword_rows),
			function(i){C[i]$startwhole:C[i]$endwhole})))
		C[singleword_rows & startword %in% multiword_pos, 
			semType := paste0('excl_s_', semType)]
	}
	setattr(D, 'annotations', C)
	D
}

remove_ancestors <- function(conceptIds, CDB = NULL,
	SNOMED = getSNOMED()){
	# Remove concepts that are ancestors of another concept from
	# a vector of concepts
	conceptIds <- as.SNOMEDconcept(conceptIds, SNOMED = SNOMED)
	if (length(conceptIds) > 0){
		# Remove all matches which are an ancestor of another match
		i <- 1
		while (i <= length(conceptIds)){
			if (!is.null(CDB)){
				ancIds <- ancestors(conceptIds[i], SNOMED = SNOMED,
					TRANSITIVE = CDB$TRANSITIVE, include_self = FALSE)
			} else {
				ancIds <- ancestors(conceptIds[i], SNOMED = SNOMED,
					include_self = FALSE)
			}
			if (length(intersect(conceptIds, ancIds)) > 0){
				conceptIds <- setdiff(conceptIds, ancIds)
				i <- 1
			} else {
				i <- i + 1
			}
		}
	}
	conceptIds
}

#' @importFrom Rdiagnosislist compose
#' @importFrom Rdiagnosislist description
refineFindings <- function(D, CDB, SNOMED){
	# Calls the 'compose' function which uses the composition lookup
	# to refine the SNOMED finding
	
	# Declare column names for CRAN check
	semType <- startwhole <- startword <- endwhole <- endword <- NULL
	conceptId <- term <- NULL

	if (is.null(CDB$COMPOSELOOKUP)){
		# No composition performed 
		return(D)
	}
	
	C <- attr(D, 'annotations')
	EXTRA <- C[0]
	root_types <- c('finding', 'disorder', 'morphologic abnormality')
	finding_rows <- C[, which(semType %in% root_types)]
	for (i in finding_rows){
		refined_conceptId <- Rdiagnosislist::compose(
			C[i]$conceptId, CDB = CDB,
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

