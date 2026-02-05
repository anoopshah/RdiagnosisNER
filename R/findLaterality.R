#### LATERALITY ####

#' @rdname findAttr
#' @family linkFunction
#' @export
findLaterality <- function(D, i){
	# Arguments:
	# D = spacy parse table containing columns: token, lemma, dep_rel,
	#   head, semType 
	# i = vector of indices of base word(s)
	
	# Possible dep_rel patterns:
	# BODY (dobj) -> LATERALITY (ROOT)
	# BODY -> LATERALITY (amod, compound)
	if (length(i) == 0){
		return(integer(0))
	}
	i_lat <- integer(0)

	# Regular expressions
	lat_phrases <- c('l', 'lt', 'left', 'r', 'rt', 'right')
	if (max(i) < nrow(D) - 2){
		# ... left and right
		if (tolower(D$token[max(i) + 1]) %in% lat_phrases &
			tolower(D$token[max(i) + 2]) == 'and' &
			tolower(D$token[max(i) + 3]) %in% lat_phrases){
			i_lat <- c(max(i) + 1, max(i) + 3)
		}
	}
	if (min(i) > 3){
		# left and right ...
		if (tolower(D$token[min(i) - 3]) %in% lat_phrases &
			tolower(D$token[min(i) - 2]) == 'and' &
			tolower(D$token[min(i) - 1]) %in% lat_phrases){
			i_lat <- c(min(i) - 1, min(i) - 3)
		}
	}
	if (min(i) > 2){
		# left/right sided ...
		if (tolower(D$token[min(i) - 2]) %in% lat_phrases &
			tolower(D$token[min(i) - 1]) %in% c('side', 'sided')){
			i_lat <- min(i) - 2
		}
	}

	# Use spacy dependency parsing
	if (length(i_lat) == 0){
		i_lat <- linkTo(D, i,
			to = c('advmod', 'amod', 'compound', 'det', 'nmod'),
			lemma_regex = '^(l|lt|left|leave|r|rt|right|bilateral|both)$')
	}
	if (length(i_lat) == 0){
		i_lat <- linkFrom(D, i, to = 'ROOT',
			from = c('dobj', 'pobj', 'compound'),
			lemma_regex = '^(l|lt|left|leave|r|rt|right|bilateral|both)$')
	}
	
	# If not laterality found, use the word immediately preceding
	# if it is a laterality
	if (length(i_lat) == 0){
		if (min(i) > 1){
			if (D$lemma[min(i) - 1] %in% lat_phrases){
				i_lat <- min(i) - 1
			}
		}
	}
	setdiff(intersect(which(D$semType == 'laterality'), i_lat), i)
}
