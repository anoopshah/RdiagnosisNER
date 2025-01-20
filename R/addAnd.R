#' Link concepts by 'and'
#'
#' Finds concepts semantically related to the index concept using the
#' conjunction 'and'. This function does not limit the output to
#' concepts of a particular semantic type, which might have to be 
#' filtered afterwards.
#' 
#' @param D spacy parse table containing columns: token, lemma, dep_rel,
#'   head, semType. Can be created by the 'showparse' function
#' @param i = vector of indices of base word(s)
#' @return integer vector of indices of linked words, or a zero length
#'   vector if no link
#' @seealso addAnd, findAllergy, findAttr, findBody, findCause,
#'   findLaterality, linkFrom, linkTo
#' @references 
#' @examples
#' D <- showparse("fracture of left femur and right humerus")
#' addAnd(D, 4)
addAnd <- function(D, i){
	# Arguments:
	# D = spacy parse table containing columns: token, lemma, dep_rel,
	#   head, semType 
	# i = vector of indices of base word(s)
	if (length(i) == 0){
		return(integer(0))
	} else if (length(i) > 1){
		# If more than one base word, recursively call the function
		# for each item individually and combine the results
		return(unique(unlist(lapply(i, function(j) addAnd(D, j)))))
	}
	
	# regular expressions
	i_and <- i
	if (min(i) > 2){
		if (paste(D$lemma[min(i) - 2], D$lemma[min(i) - 1]) %like%
			'^and (his|her|its|their|the|a|one)$'){
			i_and <- union(i_and, min(i) - 3)
		}
	}
	if (min(i) > 1){
		if (D$lemma[min(i) - 1] == 'and'){
			i_and <- union(i_and, min(i) - 2)
		}
	}
	if (max(i) < nrow(D) - 1){
		if (paste(D$lemma[max(i) + 1], D$lemma[max(i) + 2]) %like%
			'^and (his|her|its|their|the|a|one)$'){
			i_and <- union(i_and, max(i) + 3)
		}
	}
	if (max(i) < nrow(D)){
		if (D$lemma[min(i) + 1] == 'and'){
			i_and <- union(i_and, max(i) + 2)
		}
	}

	# simple conjunctions from parser
	n_and <- -1
	while (length(i_and) > n_and){
		n_and <- length(i_and)
		i_and <- union(i_and, linkTo(D, i_and,
			from = 'conj',
			to = c('conj', 'pobj', 'dobj', 'ROOT')))
		i_and <- union(i_and, linkTo(D, i_and,
			from = c('conj', 'dobj', 'pobj', 'ROOT'),
			to = 'conj'))
		i_and <- union(i_and, linkFrom(D, i_and,
			from = 'conj',
			to = c('conj', 'pobj', 'dobj', 'ROOT')))
		i_and <- union(i_and, linkFrom(D, i_and,
			from = c('conj', 'dobj', 'pobj', 'ROOT'),
			to = 'conj'))
	}
	
	# Verify that the link is 'and'
	# Find all conjunctions within the range of i_and, and check that
	# they are all 'and' (as opposed to 'or' or something else)
	if (any(D[min(i_and):max(i_and)][dep_rel == 'cc']$lemma != 'and')){
		i_and <- i
	}
	
	# NOT: -> neg
	i_neg <- D[linkTo(D, i_and, 'neg')]$head
	union(i, setdiff(i_and, i_neg))
}
