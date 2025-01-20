#### ALLERGY/INTOLERANCE TO SUBSTANCE ####
# allergic to ...
# intolerant to/of ...
# adverse reaction to ...
# ... allergy
# ... intolerance
# ... adverse reaction

#' @rdname findAttr
#' @export
findAllergy <- function(D, i){
	# Arguments:
	# D = spacy parse table containing columns: token, lemma, dep_rel,
	#   head, semType 
	# i = vector of indices of base word(s)

	if (length(i) == 0){
		return(integer(0))
	}
	
	# ALLERGY --> to/of --> SUBSTANCE (pobj)
	i_allergy <- linkTo(D, linkTo(D, i, 'prep', '^(to|of)$'), 'pobj')

	# ALLERGY -> SUBSTANCE amod, compound, dobj, nmod
	i_allergy <- c(i_allergy, linkTo(D, i, c('amod', 'nmod', 'compound', 'dobj')))
	
	# SUBSTANCE amod -> ALLERGY (e.g. 'nut allergy')
	i_allergy <- c(i_allergy, linkFrom(D, i, from = c('amod', 'dobj')))

	# NOT: SUBSTANCE -> neg
	i_allergyneg <- D[linkTo(D, i_allergy, 'neg')]$head
	
	# Add links from cause
	i_allergy <- c(i_allergy, findCause(D, i))
	
	# Return potential allergy links without negated items
	i_allergy <- setdiff(addAnd(D, i_allergy), c(i, i_allergyneg))
	
	# Limit the output to substances only
	intersect(i_allergy, which(D$semType == 'substance'))
}
