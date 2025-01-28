#### CAUSE ####

#' @rdname findAttr
#' @family linkFunction
#' @export
findCause <- function(D, i){
	# Arguments:
	# D = spacy parse table containing columns: token, lemma, dep_rel,
	#   head, semType 
	# i = vector of indices of base word(s)
	
	# Using regular expressions, as spacy dependency parsing is
	# too complex and not that helpful
	if (length(i) == 0){
		return(integer(0))
	}


	i_cause <- integer(0)

	if (max(i) < nrow(D) - 1){
		# ... by/after/follow X
		if (D$lemma[max(i) + 1] %in% c('by', 'after', 'follow')){
			i_cause <- max(i) + 2
		}
	}
	if (max(i) < nrow(D) - 2){
		# ... due to/cause by/because of X
		if (paste(D$lemma[max(i) + 1], D$lemma[max(i) + 2]) %in%
			c('due to', 'cause by', 'because of')){
			i_cause <- max(i) + 3
		}
	}
	if (max(i) < nrow(D) - 3){
		# ... as result of X
		if (paste(D$lemma[max(i) + 1], D$lemma[max(i) + 2],
			D$lemma[max(i) + 3]) %in%
			c('as result of', 'as consequence of')){
			i_cause <- max(i) + 4
		}
	}
	if (max(i) < nrow(D) - 4){
		# ... as a result of X
		if (paste(D$lemma[max(i) + 1], D$lemma[max(i) + 2],
			D$lemma[max(i) + 3], D$lemma[max(i) + 4]) %in%
			c('as a result of', 'as a consequence of')){
			i_cause <- max(i) + 5
		}
	}
	if (min(i) > 3){
		# X resulting in ...
		if (paste(D$lemma[min(i) - 2], D$lemma[min(i) - 1]) %in%
			c('result in', 'lead to', 'manifest as')){
			i_cause <- min(i) - 3
		}
	}
	if (min(i) > 2){
		# X causing ..., X related
		if (D$lemma[min(i) - 1] %in% c('cause', 'relate', 'related')){
			i_cause <- min(i) - 2
		}
		# post X ...
		if (D$lemma[min(i) - 2] == 'post'){
			i_cause <- min(i) - 1
		}
	}

	intersect(addAnd(D, i_cause), which(D$semType %in% c('finding',
		'disorder', 'organism', 'substance', 'event', 'procedure')))
}
