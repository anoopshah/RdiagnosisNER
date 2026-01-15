#' Link concepts by 'and'
#'
#' Finds concepts semantically related to the index concept using the
#' conjunction 'and'.
#' 
#' @param D spacy parse table containing columns: token, lemma, dep_rel,
#'   head, semType. Can be created by the 'showparse' function
#' @param i = vector of indices of base word(s)
#' @param semtypes = character vector of valid semantic types to return,
#'   of NULL to return any semantic types
#' @return integer vector of indices of linked words, or a zero length
#'   vector if no link
#' @seealso addAnd, findAllergy, findAttr, findBody, findCause,
#'   findLaterality, linkFrom, linkTo
#' @family linkFunction
#' @export
#' @examples
#' D <- showparse("fracture of left femur and right humerus")
#' addAnd(D, 4)
addAnd <- function(D, i, semtypes = NULL){
	# Arguments:
	# D = spacy parse table containing columns: token, lemma, dep_rel,
	#   head, semType 
	# i = vector of indices of base word(s)
	
	# Declare column names for CRAN check
	dep_rel <- NULL
	
	if (length(i) == 0){
		return(integer(0))
	} else if (length(i) > 1){
		# If more than one base word, recursively call the function
		# for each item individually and combine the results
		return(unique(unlist(lapply(i, function(j){
			addAnd(D, j, semtypes)
		}))))
	}
	
	# regular expressions
	i_and <- i
	prep <- c('his', 'her', 'its', 'their', 'the', 'a', 'one')
	if (min(i) >= 4){ # yyy and his xxx
		if (tolower(D$lemma[min(i) - 2]) == 'and' &
			tolower(D$lemma[min(i) - 1]) %in% prep){
			i_and <- union(i_and, min(i) - 3)
		}
	}
	if (min(i) >= 3){ # yyy and xxx
		if (tolower(D$lemma[min(i) - 1]) == 'and'){
			i_and <- union(i_and, min(i) - 2)
		}
	}
	if (min(i) >= 5){ # yyy , yyy and xxx
		if (D$lemma[min(i) - 3] == ',' &
			tolower(D$lemma[min(i) - 1]) == 'and'){
			i_and <- union(i_and, min(i) - c(2, 4))
		}
	}
	if (min(i) >= 6){ # yyy , yyy yyy and xxx
		if (D$lemma[min(i) - 4] == ',' &
			tolower(D$lemma[min(i) - 1]) == 'and'){
			i_and <- union(i_and, min(i) - c(2, 3, 5))
		}
	}
	
	if (max(i) <= nrow(D) - 3){ # xxx and his yyy
		if (tolower(D$lemma[max(i) + 1]) == 'and' &
			tolower(D$lemma[max(i) + 2]) %in% prep){
			i_and <- union(i_and, max(i) + 3)
		}
	}
	if (max(i) <= nrow(D) - 2){ # xxx and yyy
		if (tolower(D$lemma[max(i) + 1]) == 'and'){
			i_and <- union(i_and, max(i) + 2)
		}
	}
	if (max(i) <= nrow(D) - 4){ # xxx , yyy and yyy
		if (D$lemma[max(i) + 1] == ',' &
			tolower(D$lemma[max(i) + 3]) == 'and'){
			i_and <- union(i_and, max(i) + c(2, 4))
		}
	}
	if (max(i) <= nrow(D) - 5){ # xxx , yyy yyy and yyy
		if (D$lemma[max(i) + 1] == ',' &
			tolower(D$lemma[max(i) + 4]) == 'and'){
			i_and <- union(i_and, max(i) + c(2, 3, 5))
		}
	}

	# simple conjunctions from parser - not so useful for this
	# application so currently not using
#~ 	n_and <- -1
#~ 	while (length(i_and) > n_and){
#~ 		n_and <- length(i_and)
#~ 		i_and <- union(i_and, linkTo(D, i_and,
#~ 			from = 'conj',
#~ 			to = c('conj', 'pobj', 'dobj', 'ROOT')))
#~ 		i_and <- union(i_and, linkTo(D, i_and,
#~ 			from = c('conj', 'dobj', 'pobj', 'ROOT'),
#~ 			to = 'conj'))
#~ 		i_and <- union(i_and, linkFrom(D, i_and,
#~ 			from = 'conj',
#~ 			to = c('conj', 'pobj', 'dobj', 'ROOT')))
#~ 		i_and <- union(i_and, linkFrom(D, i_and,
#~ 			from = c('conj', 'dobj', 'pobj', 'ROOT'),
#~ 			to = 'conj'))
#~ 	}
	
	# Verify that the link is 'and'
	# Find all conjunctions within the range of i_and, and check that
	# they are all 'and' (as opposed to 'or' or something else)
#~ 	if (any(D[min(i_and):max(i_and)][dep_rel == 'cc']$lemma != 'and')){
#~ 		i_and <- i
#~ 	}
	
	# NOT: -> neg
	i_neg <- D[linkTo(D, i_and, 'neg')]$head
	
	i_and <- union(i, setdiff(i_and, i_neg))
	if (is.null(semtypes)){
		return(i_and)
	} else {
		return(intersect(i_and, which(D$semType %in% semtypes)))
	}
}
