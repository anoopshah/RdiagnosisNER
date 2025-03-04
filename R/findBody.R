#### BODY STRUCTURE ####

#' @rdname findAttr
#' @family linkFunction
#' @export
findBody <- function(D, i){
	# Arguments:
	# D = spacy parse table containing columns: token, lemma, dep_rel,
	#   head, semType 
	# i = vector of indices of base word(s)
	
	# Find potential body parts linked to a disease or finding
	if (length(i) == 0){
		return(integer(0))
	}
	
	i_body <- integer(0)
	# Using regular expression
	# e.g. fracture of femur, pain in foot
	if (max(i) < nrow(D) - 1){
		if (tolower(D$lemma[max(i) + 1]) %in% c('of', 'on', 'in')){
			i_body <- max(i) + 2
		}
	} 
	
	# DISEASE dobj -> ROOT -> prep (of|on|in) -> BODY pobj
	i_body <- c(i_body, linkTo(D, linkTo(D,
		linkFrom(D, i, to = 'ROOT', from = 'dobj'),
		'prep', '^(of|on|in)$'), 'pobj'))
	
	# DISEASE -> prep (of|on|in) -> BODY pobj
	i_body <- c(i_body, linkTo(D,
		linkTo(D, i, 'prep', '^(of|on|in)$'), 'pobj'))

	# DISEASE -> BODY amod, compound, dobj, nmod
	i_body <- c(i_body, linkTo(D, i, c('amod', 'nmod', 'compound', 'dobj')))
	
	# DISEASE amod -> BODY ROOT (e.g. 'bruised elbow')
	i_body <- c(i_body, linkFrom(D, i, from = c('amod', 'compound')))

	# DISEASE ROOT -> BODY nsubj (e.g. 'head fracture',
	# 'fracture of xxx and femur')
	i_body <- c(i_body, linkTo(D, i, to = c('nsubj', 'conj'),
		from = 'ROOT'))

	# NOT: BODY -> neg
	i_bodyneg <- D[linkTo(D, i_body, 'neg')]$head
	
	# Return potential body sites without negated items and without
	# original (finding) concept
	i_body <- setdiff(i_body, union(i, i_bodyneg))
	i_body <- setdiff(addAnd(D, i_body), union(i, i_bodyneg))
	
	# Limit to body structure or laterality semantic types - note that
	# laterality is linked to body structure before body structure
	# is linked to finding
	intersect(i_body, which(D$semType %in% c('body structure',
		'laterality')))
}
