#' Test the text linking functions
#'
#' Test a single example or use a dataset to carry out a series of tests
#'
#' @param testlink_data data.frame containing link function tests
#'   in the format
#'   one row per test, with columns: linkFunction (link function),
#'   text, semTypes (semantic type of each word in the format of
#'   semTypes as described below), index_start (start position of index
#'   word for linkage,
#'   index_end (end position of index word for linkage, same as 
#'   index_start if a single word), expected
#'   (correct output in the format: "# word, # word2|# alt" where # is the
#'   index number of the word linked to, word/word2/alt are the words
#'   linked to, and | separates different options for correct outputs).
#'   If NULL, the provided dataset testlink_data will be used.
#' @param linkFunction a function or character vector of length 1
#'   specifying the name of the link function to test which can be
#'   retrieved with get (one of 'findLaterality', 'findAttr',
#'   'findBody', 'findCause', or 'findAllergy')
#' @param text phrase to test
#' @param semTypes either a character vector containing the semantic
#'   type of each word in text, or a single string of length 1
#'   and number of characters equal to number of words in text,
#'   containing letters that represent the semantic type of a word:
#'   (space) = (any semantic type), F = finding, D = disorder,
#'   B = body structure, L = laterality, O = organism, S = substance,
#'   P = procedure, Z = severity, Q = qualifier value
#' @param i integer vector denoting the index number or range of word
#'   that the link is in relation to
#' @param expected character vector where each element is a possible
#'   correct output in the format: "# word, # word2" where # is the
#'   index number of the word linked to, and word/word2 are the words
#'   linked to
#' @return testlink returns a single string equal to TRUE if the
#'   output is equal to expected, and the actual output
#'   if there is a difference. testlink_from_data returns a character
#'   vector containing the result for each row 
#' @seealso testlink_data
#' @export
#' @examples
#' data(testlink_data)
#' testlink_from_data(testlink_data)
testlink_from_data <- function(testlink_data = NULL){
	
	# Declare column names for CRAN check
	semType <- NULL
	
	if (is.null(testlink_data)){
		data('testlink_data', envir = environment())
	}
	D <- as.data.table(testlink_data)
	D_expected <- strsplit(D$expected, '\\|')
	if (identical(D_expected, character(0))){
		D_expected <- ''
	}
	out <- rep(NA_character_, nrow(D))
	for (i in 1:nrow(D)){
		try(out[i] <- testlink(linkFunction = D$linkFunction[i], 
			text = D$text[i], semTypes = D$semTypes[i],
			i = D$index_start[i]:D$index_end[i],
			expected = D_expected[[i]]))
	}
	out
}

#' @rdname testlink_from_data
#' @export
testlink <- function(linkFunction = c('addAnd',
	'findLaterality', 'findAttr', 'findBody', 'findCause',
	'findAllergy'), text, semTypes, i, expected = NULL){
		
	# Declare column names for CRAN check
	semType <- NULL
	
	# Function to test semantic linkages
	# semType can be a vector of semantic types or an abbreviated
	# form where each character represents the semantic type of a word
	# (space) = (any semantic type)
	# F = finding
	# D = disorder
	# B = body structure
	# L = laterality
	# O = organism
	# S = substance
	# P = procedure
	# Z = severity
	# Q = qualifier value
	semTypesLookup <- c('finding', 'disorder', 
		'body structure', 'laterality', 'organism', 'substance',
		'procedure', 'severity', 'qualifier value')
	names(semTypesLookup) <- c('F', 'D', 'B', 'L', 'O', 'S', 'P',
		'Z', 'Q')
	if (is.character(linkFunction)){
		linkFunction <- get(linkFunction[1])
	}
	D <- showparse(text)
	if (length(semTypes) == 1){
		if (nchar(semTypes) == nrow(D)){
			semTypes <- semTypesLookup[strsplit(semTypes, '')[[1]]]
		}
	} else if (length(semTypes) == nrow(D)){
		# keep as is
	} else {
		semTypes <- NULL
	}
	D[, semType := semTypes]
	pos1 = sort(linkFunction(D, i))
	out <- paste(pos1, D$token[pos1], collapse = ', ')
	# change output to be always text
	if (out %in% expected |
		(identical(out, '') & identical(expected, character(0)))){
		'TRUE'
	} else {
		setattr(out, 'details', D)
		out
	}
}

#' Test file for linking functions
#'
#' A table of tests of link functions
#' 
#' @name testlink_data
#' @aliases testlink_data
#' @docType data
#' @importFrom utils data
#' @usage data(testlink_data)
#' @format An object of class \code{"data.table"} and
#'   \code{"data.frame"}
#' @keywords datasets
#'
#' @details
#' \describe{
#'   \item{linkFunction}{ character: function to test; one of
#'     addAnd, findLaterality, findAttr, findBody, findCause,
#'     or findAllergy}
#'   \item{text}{ character: text to test}
#'   \item{semTypes}{ character: string with number of characters
#'     equal to number of words in text,
#'     containing letters that represent the semantic type of a word:
#'     (space) = (any semantic type), F = finding, D = disorder,
#'     B = body structure, L = laterality, O = organism, S = substance,
#'     P = procedure, Z = severity, Q = qualifier value}
#'   \item{index_start}{ integer: position of the first word that
#'     the link is in relation to}
#'   \item{index_end}{ integer: position of the last word that the
#'     link is in relation to; same as index_start if it is a single
#'     word}
#'   \item{expected}{ character: correct output in the format:
#'     "# word, # word2|# alt" where # is the
#'     index number of the word linked to, word/word2/alt are the words
#'     linked to, and | separates different options for correct outputs}
#' }
#' 
#' @seealso testlink, testlink_from_data
#' @examples
#' # Show properties of the testlink_data table
#' data('testlink_data')
#' str(testlink_data)
"testlink_data"
