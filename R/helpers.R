#### R VERSIONS OF MEDCAT FUNCTIONALITY ####

# Create a data.table containing the parse tree for a sentence 
# Output: data.table with columns token, lemma, dep_rel, head

#' Parse a phrase using spacy parse  
#'
#' Applies the 'spacy parse' function to a phrase.
#' 
#' @param x text to process with spacy parse
#' @param min_len_normalize tokenswith fewer than this number of
#'   characters will not be lemmatised
#' @return data.table with columns token, lemma, dep_rel
#'   (dependency relation) and head (index number of token that the
#'   dependency relates to)
#' @import data.table
#' @import bit64
#' @import data.table
#' @import Rdiagnosislist
#' @seealso 
#' @references 
#' @examples
#' D <- showparse("fracture of left femur and right humerus")
#' D
#' @export
showparse <- function(x, min_len_normalize = 5){
	OUT <- as.data.table(spacy_parse(x, entity = TRUE,
		remove_punct = TRUE, dependency = TRUE, lemma = TRUE,
		pos = TRUE, output = 'data.frame'))[,
		list(token, lemma, dep_rel, head_token_id)]
	OUT[nchar(token) < min_len_normalize, lemma := token]
	OUT
}

#' Add lemmatized versions of terms to a concept database (CDB)
#'
#' Adds a 'lemma' column to the relevant tables within the concept
#' database, using the spacy lemmatizer, in order to assist term
#' detection. The function does not
#' lemmatize terms which are partially or fully capitalised
#' as these need to be matched in a case sensitive manner.
#' 
#' @param CDB concept database environment
#' @param tablenames which data tables to add lemmatized terms
#' @param ignore_if_already TRUE or FALSE, whether to skip if
#'   a table already has a lemma column
#' @param min_len_normalize tokenswith fewer than this number of
#'   characters will not be lemmatised
#' @return CDB environment with lemma columns added to relevant tables
#' @seealso 
#' @references 
#' @examples
addLemmaToCDB <- function(CDB, tablenames = c('FINDINGS', 
	'QUAL', 'BODY', 'LATERALITY', 'SEVERITY', 'CAUSES', 'MORPH'),
	ignore_if_already = TRUE, min_len_normalize = 5){
	for (i in tablenames){
		TABLE <- get(i, envir = CDB)
		if ((!ignore_if_already) | (!('lemma' %in% names(TABLE)))){
			message('Lemmatizing ', i)
			TABLE[, lemma := ifelse(term == tolower(term) &
				nchar(term) > min_len_normalize,
				paste0(' ', paste(spacy_parse(sub('^ +| +$', '', term))$lemma,
				collapse = ' '), ' '), term), by = .(conceptId, term)]
			setindexv(TABLE, 'lemma')
			assign(i, TABLE, envir = CDB)
		}
	}
	return(CDB)
}


#' Find SNOMED CT concepts matching a portion of text
#'
#' This is a simple dictionary-based named entity recognition (NER)
#' function. If there are multiple concepts with the same term,
#' this function may return multiple matches.
#' 
#' @param text text phrase to match
#' @param lemma lemma to match
#' @param CDB concept database environment
#' @param SNOMED SNOMED CT dictionary environment
#' @return unique SNOMEDconcept vector of matches
#' @seealso 
#' @references 
#' @examples
findConceptMatch <- function(text, lemma = NULL, CDB, SNOMED){
	# A simple NER matching protocol = seeks a SNOMED CT match for
	# a text. The text needs to be preceded and followed by spaces
	# and must be lower case.
	if (is.null(lemma)){
		thelemma <- paste0(' ', paste(text, collapse = ' '), ' ')
	} else {
		thelemma <- paste0(' ', paste(lemma, collapse = ' '), ' ')
	}
	text <- c(paste0(' ', paste(text, collapse = ' '), ' '),
		paste0(' ', paste(tolower(text), collapse = ' '), ' '))
	# Return a vector of matched concepts
	unique(as.SNOMEDconcept(c(
		CDB$FINDINGS[term %in% text | lemma %in% thelemma]$conceptId,
		CDB$QUAL[term %in% text | lemma %in% thelemma]$conceptId,
		CDB$BODY[term %in% text | lemma %in% thelemma]$conceptId,
		CDB$LATERALITY[term %in% text | lemma %in% thelemma]$conceptId,
		CDB$SEVERITY[term %in% text | lemma %in% thelemma]$conceptId,
		CDB$CAUSES[term %in% text | lemma %in% thelemma]$conceptId,
		CDB$MORPH[term %in% text | lemma %in% thelemma]$conceptId),
		SNOMED = SNOMED))
}

#' Parse a sentence using spacy and find annotations
#'
#' Parses a sentence using showparse, and finds annotations using
#' findConceptMatch.
#' @param text text phrase to match
#' @param CDB concept database environment
#' @param SNOMED SNOMED CT dictionary environment
#' @param wordlimit maximum number of words in a phrase to attempt
#'  to match to a SNOMED concept
#' @return spacy parse object (output of showparse) with an attribute
#'  'annotations' which is a data.table with columns conceptId,
#'  startword (index of first word of matched phrase),
#'  endword (index of last word of matched phrase),
#'  startwhole (index of first word of entire concept, in this case
#'  same as startword), endwhole (index of last word of entire concept,
#'  in this case same as endword), semType (semantic type of
#'  matched SNOMED CT concept), due_to (linked cause concept),
#'  causing (linked consequence concept), attributes (list of 
#'  concept attributes), link_to (concept that this concept is
#'  an attribute of), laterality (SNOMED CT concept for laterality of
#'  of concept
#' @seealso showparse
#' @references 
#' @examples
parseSentence <- function(text, CDB, SNOMED, wordlimit = 6,
	keep_all_matches = FALSE){
	# Option to keep all matches - not recommended as some small
	# word matches may be incorrect in context
	as.SNOMEDconcept('', SNOMED = SNOMED) -> zeroconcept
	D <- showparse(text)
	# D is the parse table for text (one row per word)
	D[, semType := 'none']
	# C is the annotations table being created (one row per SNOMED CT
	# concept extracted from the text)
	# startword, endword are the start and end of this concept
	# startwhole, endwhole are the start and end of the final concept
	#   when combined with attributes
	C <- data.table(conceptId = zeroconcept,
		startword = integer(0), endword = integer(0),
		startwhole = integer(0), endwhole = integer(0),
		semType = character(0), due_to = list(zeroconcept),
		causing = list(zeroconcept),
		attributes = list(zeroconcept), link_to = integer(0),
		laterality = zeroconcept)
	# Linking by token and lemma
	# Search for matches up to (wordlimit) words
	i = 1
	while (i <= nrow(D)){
		matches <- lapply(1:wordlimit, function(x){
			if (i + x - 1 > nrow(D)){
				character(0)
			} else {
				as.character(findConceptMatch(D[i:(i + x - 1)]$token,
					D[i:(i + x - 1)]$lemma, CDB, SNOMED))
			}
		})
		keep <- which(sapply(matches, length) > 0)
		if (length(keep) > 0){
			if (!keep_all_matches) keep <- max(keep)
			C <- rbind(C, data.table(conceptId = as.SNOMEDconcept(
				unlist(matches[keep]), SNOMED = SNOMED),
				startword = i, startwhole = i,
				endword = i + keep - 1, endwhole = i + keep - 1),
				fill = TRUE)
			if (keep_all_matches){
				i <- i + 1
			} else {
				i <- i + keep
			}
		} else {
			i <- i + 1
		}
	}
	
	if (nrow(C) > 0){
		C[, semType := CDB$SEMTYPE[C, on = 'conceptId']$semType]
		C[, term := description(conceptId, SNOMED = SNOMED)$term]
		for (i in 1:nrow(C)){
			D[C[i]$startword:C[i]$endword, semType := C[i]$semType]
		}
	}
	
	# The annotations (concepts found) are stored in the 'annotations'
	# attribute
	setattr(D, 'annotations', C)
	# Now to refine the concept type
	return(D)
}

# Helper function for R to convert a list into plain vector of
# SNOMED CT concepts
unique_unlist_int64 <- function(conceptIds){
	as.SNOMEDconcept(unique(bit64::as.integer64(
		unlist(lapply(conceptIds, as.character)))), SNOMED = SNOMED)
}
