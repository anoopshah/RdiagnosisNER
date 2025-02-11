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
#' @import spacyr
#' @export
#' @importFrom spacyr spacy_parse
#' @import Rdiagnosislist
#' @seealso parseSentence
#' @export
#' @examples
#' D <- showparse("fracture of left femur and right humerus")
#' D
showparse <- function(x, min_len_normalize = 5){
	
	# Declare column names for CRAN check
	token <- lemma <- dep_rel <- head_token_id <- NULL
	
	OUT <- as.data.table(spacyr::spacy_parse(x, entity = TRUE,
		remove_punct = TRUE, dependency = TRUE, lemma = TRUE,
		pos = TRUE, output = 'data.frame'))[,
		list(token, lemma, dep_rel, head_token_id)]
	OUT[nchar(token) < min_len_normalize, lemma := token]
	OUT
}

#' Add a LEMMA table to a concept database (CDB)
#'
#' Adds a 'LEMMA' data.table containing the original term and lemmatized
#' version for fast concept lookup. The function uses the spacy
#' lemmatizer. The function does not
#' lemmatize terms which are partially or fully capitalised
#' as these need to be matched in a case sensitive manner.
#' 
#' @param CDB concept database environment
#' @param tablenames which data tables to add lemmatized terms
#' @param min_len_normalize tokens with fewer than this number of
#'   characters will not be lemmatised
#' @return CDB environment with an additional 'LEMMA' table. If it
#'   already exists it will be over-written
#' @importFrom spacyr spacy_parse
#' @importFrom data.table setindexv
#' @importFrom data.table setkeyv
#' @seealso findConceptMatch
#' @export
#' @importFrom data.table data.table
#' @importFrom data.table copy
#' @examples
#' # Not run
#' # CDB <- addLemmaToCDB(CDB)
addLemmaToCDB <- function(CDB, tablenames = c('FINDINGS', 
	'QUAL', 'BODY', 'LATERALITY', 'SEVERITY', 'CAUSES', 'MORPH'),
	min_len_normalize = 5){
		
	# Declare column names for CRAN check
	lemma <- term <- conceptId <- NULL
	
	LEMMA <- data.table(term = character(0), 
		conceptId = SNOMEDconcept(character(0)))
	for (i in tablenames){
		message('Lemmatizing ', i)
		TABLE <- copy(get(i, envir = CDB)[, list(conceptId, term)])
		LEMMA <- rbind(LEMMA, TABLE)
	}
	LEMMA <- LEMMA[!duplicated(LEMMA)]
	LEMMA[, lemma := ifelse(term == tolower(term) &
		nchar(term) > min_len_normalize,
		paste0(' ', paste(spacyr::spacy_parse(sub('^ +| +$', '',
		term))$lemma, collapse = ' '), ' '), term),
		by = list(conceptId, term)]
	data.table::setkeyv(LEMMA, 'lemma')
	data.table::setindexv(LEMMA, 'term')
	CDB$LEMMA <- LEMMA
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
#' @export
#' @seealso showparse, parseSentence, addLemmaToCDB
findConceptMatch <- function(text, lemma = text, CDB, SNOMED){
	# A simple NER matching protocol = seeks a SNOMED CT match for
	# a text. The text needs to be preceded and followed by spaces
	# and must be lower case.
	
	term <- conceptId <- NULL

	thelemma <- paste0(' ', paste(lemma, collapse = ' '), ' ')
	theterm <- c(paste0(' ', paste(text, collapse = ' '), ' '),
		paste0(' ', paste(tolower(text), collapse = ' '), ' '))
	
	# Return a vector of matched concepts
	union(CDB$LEMMA[term %in% theterm]$conceptId,
		CDB$LEMMA[lemma %in% thelemma]$conceptId)
}

#' Parse a sentence using spacy and find SNOMED CT annotations
#'
#' Parses a sentence using showparse, and finds SNOMED CT annotations
#' using findConceptMatch.
#' @param text text phrase to match
#' @param CDB concept database environment
#' @param SNOMED SNOMED CT dictionary environment
#' @param wordlimit maximum number of words in a phrase to attempt
#'  to match to a SNOMED concept
#' @param keep_all_matches whether to keep 
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
#' @seealso showparse, findConceptMatch
#' @export
#' @importFrom data.table data.table
#' @importFrom data.table as.data.table
#' @importFrom data.table is.data.table
#' @examples
#' # Create CDB for NER
#' data.table::setDTthreads(threads = 1)
#' require(Rdiagnosislist)
#'
#' SNOMED <- sampleSNOMED()
#' miniCDB <- createCDB(SNOMED = SNOMED)
#' miniCDB <- addLemmaToCDB(miniCDB)
#'
#' findConceptMatch('HF', CDB = miniCDB, SNOMED = SNOMED)
#' # [1] "84114007 | Heart failure (disorder)"
#'
#' A <- parseSentence('He has heart failure', CDB = miniCDB,
#'   SNOMED = SNOMED)
#' A
#' #      token   lemma  dep_rel head_token_id  semType
#' #     <char>  <char>   <char>         <num>   <char>
#' # 1:      He      He    nsubj             2     none
#' # 2:     has     has     ROOT             2     none
#' # 3:   heart   heart compound             4 disorder
#' # 4: failure failure     dobj             2 disorder
#'
#' attr(A, 'annotations')
#' #          conceptId startword endword startwhole endwhole  semType due_to
#' #    <SNOMEDconcept>     <num>   <num>      <num>    <num>   <char> <list>
#' # 1:        84114007         3       4          3        4 disorder [NULL]
#' #    causing attributes link_to      laterality                     term
#' #     <list>     <list>   <int> <SNOMEDconcept>                   <char>
#' # 1:  [NULL]     [NULL]      NA            <NA> Heart failure (disorder)
parseSentence <- function(text, CDB, SNOMED, wordlimit = 6,
	keep_all_matches = FALSE){
	# Option to keep all matches - not recommended as some small
	# word matches may be incorrect in context
	
	# Declare column names for CRAN checks
	semType <- term <- conceptId <- NULL
	
	D <- showparse(text)
	# D is the parse table for text (one row per word)
	D[, semType := 'none']
	# C is the annotations table being created (one row per SNOMED CT
	# concept extracted from the text)
	# startword, endword are the start and end of this concept
	# startwhole, endwhole are the start and end of the final concept
	#   when combined with attributes
	C <- data.table(conceptId = SNOMEDconcept(character(0)),
		startword = integer(0), endword = integer(0),
		startwhole = integer(0), endwhole = integer(0),
		semType = character(0), due_to = list(0),
		causing = list(0),
		attributes = list(0), link_to = integer(0),
		laterality = SNOMEDconcept(character(0)), term = character(0))
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
