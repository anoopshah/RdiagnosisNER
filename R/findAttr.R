#' Find attributes of a concept
#'
#' Finds concepts semantically related to the index concept.
#' For findAllergy, the semantic type is limited to substances.
#' For findBody, the semantic type is limited to body structures or
#'   laterality.
#' For findLaterality, the semantic type is limited to laterality.
#' For findCause, the semantic type must be one of finding,
#'   disorder, organism, substance, event or procedure.
#' 
#' @param D spacy parse table containing columns: token, lemma, dep_rel,
#'   head, semType 
#' @param i = vector of indices of base word(s)
#' @return integer vector of indices of linked words, or a zero length
#'   vector if no link
#' @seealso addAnd, findAllergy, findAttr, findBody, findCause,
#'   findLaterality, linkFrom, linkTo
#' @references 
#' @examples
#' D <- showparse("ulcers of the left big and second toes due to diabetes")
#' findAttr(D, 4)
#' findBody(D, 4)
#' findCause(D, 4)
#' findAllergy(D, 4)
findAttr <- function(D, i){
	# Arguments:
	# D = spacy parse table containing columns: token, lemma, dep_rel,
	#   head, semType 
	# i = vector of indices of base word(s)
	
	# Possible dep_rel patterns:
	# THING -> DESCRIPTOR (amod, compound, conj)
	# THING -> DESCRIPTOR (amod, compound) -> DESCRIPTOR (conj)
	if (length(i) == 0){
		return(integer(0))
	}
	i_desc <- linkTo(D, i, c('amod', 'compound', 'nmod'))
	setdiff(addAnd(D, setdiff(i_desc, i)), i)
}
