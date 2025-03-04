#### FUNCTIONS TO FIND POTENTIAL LINKS VIA SPACY PARSING ####
# then validate by type of destination 

#' Find linked concepts
#'
#' Finds concepts semantically related to the index concept. These
#' functions are called by addAnd, findAllergy, findAttr etc.
#' 
#' @param D spacy parse table containing columns: token, lemma, dep_rel,
#'   head, semType 
#' @param i vector of indices of base word(s)
#' @param from vector of dependency relations to map from, or "ANY"
#' @param lemma_regex regular expression condition for lemma
#' @param to vector of dependency relations to map to, or "ANY"
#' @return integer vector of indices of linked words, or a zero length
#'   vector if no link
#' @seealso addAnd, findAllergy, findAttr, findBody, findCause,
#'   findLaterality, linkFrom, linkTo
#' @export
#' @family linkFunction
#' @examples
#' D <- showparse("ulcers of the left big and second toes due to diabetes")
linkFrom <- function(D, i, from = 'ANY', lemma_regex = '.*', to = 'ANY'){
	# Arguments:
	# D = spacy parse table containing columns: token, lemma, dep_rel,
	#   head, semType
	# i = vector of indices of base word(s)
	# from = set of possible dep_rel for base word(s)
	# to = set of possible dep_rel for target word(s)
	# lemma_regex = regular expression to match each target lemma
	#
	# Returns the indices of the targets, or integer(0) if not found
	if (nrow(D) == 0 | length(i) == 0){
		return(integer(0))
	}
	if (identical(from, 'ANY')){
		valid_from <- i
	} else {
		valid_from <- intersect(i, which(D$dep_rel %in% from))
	} # valid_from is indices of subset of base which are valid
	if (length(valid_from) == 0){
		return(integer(0))
	}
	if (identical(to, 'ANY')){
		valid_to <- D[valid_from]$head
	} else {
		valid_to <- intersect(D[valid_from]$head, which(D$dep_rel %in% to))
	} # valid_from is indices of subset of target which are valid
	if (length(valid_to) == 0){
		return(integer(0))
	}
	intersect(valid_to, union(which(D$lemma %like% lemma_regex),
		which(tolower(D$lemma) %like% lemma_regex)))
}

#' @rdname linkFrom
#' @family linkFunction
#' @export
linkTo <- function(D, i, to = 'ANY', lemma_regex = '.*', from = 'ANY'){
	# Arguments:
	# D = spacy parse table containing columns: token, lemma, dep_rel,
	#   head, semType
	# i = vector of indices of base word(s)
	# from = set of possible dep_rel for base word(s)
	# to = set of possible dep_rel for target word(s)
	# lemma_regex = regular expression to match each target lemma
	#
	# Returns the indices of the targets, or integer(0) if not found
	if (nrow(D) == 0 | length(i) == 0){
		return(integer(0))
	}
	if (identical(from, 'ANY')){
		valid_from <- unique(i)
	} else {
		valid_from <- intersect(i, which(D$dep_rel %in% from))
	} # valid_from is indices of subset of base which are valid
	if (length(valid_from) == 0){
		return(integer(0))
	}
	if (identical(to, 'ANY')){
		valid_to <- which(D$head %in% valid_from)
	} else {
		valid_to <- which(D$head %in% valid_from & D$dep_rel %in% to)
	} # valid_from is indices of subset of target which are valid
	if (length(valid_to) == 0){
		return(integer(0))
	}
	intersect(valid_to, union(which(D$lemma %like% lemma_regex),
		which(tolower(D$lemma) %like% lemma_regex)))
}

