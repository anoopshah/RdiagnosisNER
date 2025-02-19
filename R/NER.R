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
#' @param unigram_blacklist data.frame or data.table with columns conceptId
#'   (character or integer64) and term (character, non-case sensitive)
#'   which
#'   contains a list of one-character terms and conceptIds that can be
#'   used for concept composition but should be removed in the final
#'   output because they may cause errors.
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
#' @importFrom Rdiagnosislist sampleSNOMED
#' @examples
#' data.table::setDTthreads(threads = 1)
#' require(Rdiagnosislist)
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
NERsentence <- function(text, CDB, SNOMED, noisy = TRUE,
	unigram_blacklist = NULL){
	
	# Declare data.table variables for R CRAN check
	startwhole <- endwhole <- conceptId <- term <- NULL
	token_id <- semType <- NULL
	
	unigram_blacklist <- process_blacklist(unigram_blacklist,
		SNOMED = SNOMED)
	
	# Avoid spacy parse error with blank string
	if (text == ''){ text <- ' '}
	
	# Apply the spacy parser and do lookup for SNOMED CT concepts
	D <- parseSentence(text, CDB, SNOMED)
	
	# Add laterality and body site links
	D <- addLateralityBodyLinks(D, CDB, SNOMED)
	
	# Add other attributes based on dependency parser
	D <- addAttributeLinks(D, CDB, SNOMED)
	
	# Add attributes based on proximity
	D <- addProximityLinks(D, CDB, SNOMED)
	
	# Transfer laterality
	D <- addLateralityTransfer(D, CDB, SNOMED)
	
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
				by = list(conceptId, term)][, list(conceptId, term,
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
	
	if (noisy) cat('\nRemove ancestors of other concepts:\n')

	# Remove concepts that are ancestors of another concept (to tidy
	# the output and leave only the most specific concepts)
	D <- removeAncestorsD(D, CDB, SNOMED)
	
	if (noisy){
		print(showannotations(D))
		cat('\nRemove single word overlapped findings:\n')
	}
	
	# Remove single word overlapped findings
	D <- removeSingleWordOverlappedFindingsD(D)
	
	if (noisy){
		print(showannotations(D))
		cat('\nRemove multiple or blacklisted single word findings:\n')
	}
	
	# Remove multiple or blacklisted single word findings
	D <- removeAmbiguousSingleWordFindingsD(D, unigram_blacklist)
	
	if (noisy) print(showannotations(D))
	
	D[, token_id := 1:.N]
	F <- attr(D, 'annotations')[
		semType %in% c('finding', 'disorder'), list(conceptId, term)]
	F <- F[!duplicated(F)]
	setattr(D, 'findings', F)
	if (noisy){
		cat('\nFinal output:\n')
		print(attr(D, 'findings'))
	}
	invisible(D)
}

#' @rdname NERsentence
#' @export
NERdocument <- function(text, CDB, SNOMED, unigram_blacklist = NULL){
	
	# Declare data.table variables for R CRAN check
	sentence <- semType <- conceptId <- term <- NULL
	
	unigram_blacklist <- process_blacklist(unigram_blacklist,
		SNOMED = SNOMED)
	
	sentences <- strsplit(text, '\\\\n|\\. ')[[1]]
	D <- lapply(seq_along(sentences), function(i){
		x <- NERsentence(sentences[i], CDB, SNOMED,
			noisy = FALSE, unigram_blacklist)
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
	CDB, SNOMED, unigram_blacklist = NULL){
	# returns findings
	
	# Declare data.table variables for R CRAN check
	semType <- conceptId <- term <- NULL
	
	unigram_blacklist <- process_blacklist(unigram_blacklist,
		SNOMED = SNOMED)
	
	if (length(ids) != length(texts)){
		stop('ids must the same length as texts')
	}
	D <- lapply(seq_along(ids), function(i){
		NERdocument(texts[i], CDB, SNOMED, unigram_blacklist)
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

process_blacklist <- function(unigram_blacklist = NULL, SNOMED = SNOMED){
	if (is.null(unigram_blacklist)){
		return(NULL)
	}
	if ('unigram_blacklist' %in% class(unigram_blacklist)){
		return(unigram_blacklist)
	}
	unigram_blacklist <- as.data.table(unigram_blacklist)
	unigram_blacklist[, conceptId := as.SNOMEDconcept(conceptId,
		SNOMED = SNOMED)]
	unigram_blacklist[, term := tolower(gsub('^ *| *$', '', term))]
	unigram_blacklist <- unigram_blacklist[!duplicated(unigram_blacklist)]
	setkeyv(unigram_blacklist, 'term')
	setindexv(unigram_blacklist, 'conceptId')
	setattr(unigram_blacklist, 'class', c('unigram_blacklist',
		'data.table', 'data.frame'))
	unigram_blacklist
}

