#' Evaluate the performance of named entity recognition compared to
#' a gold standard
#'
#' Compares the extraction of diagnosis data to a gold standard
#' in the following way. Firstly, the comparison considers only unique
#' mentions of a concept (repeats are ignored). Secondly, the comparison
#' ignores ancestors of any concept. For example, if a document contains
#' both 'diabetes mellitus' (ancestor) and 'diabetic retinopathy'
#' (descendant which implies the presence of diabetes mellitus), the
#' results will be the same whether or not the diabetes mellitus concept
#' is returned by the algorithm.
#'
#' @param actual data.frame containing columns id (document identifier)
#'   and conceptId, e.g. output of NERcorpus
#' @param goldstandard data.frame containing columns id (document
#'   identifier) and conceptId (character, which can contain multiple
#'   concepts separated by |). If the conceptId column is integer64, it
#'   is assumed that there is only one correct SNOMED CT concept for
#'   each mention.
#' @param relaxed Boolean TRUE or FALSE. TRUE means that any ancestor
#'   of a gold standard concept is considered correct. FALSE means that
#'   only exact concepts are counted
#' @param aggregate Boolean TRUE or FALSE. TRUE means that all counts
#'   aggregated across all documents before calculating precision,
#'   recall and F1. FALSE returns results per document
#' @param SNOMED environment containing a SNOMED dictionary
#' @param subset SNOMEDconcept vector specifying which concepts
#'   to include in this evaluation (e.g. only disorders)
#' @return data.table with columns precision, recall, F1, and also
#'   an id column if aggregate = FALSE.
#' @seealso NERcorpus
#' @export
#' @importFrom data.table data.table
#' @examples
#' require(Rdiagnosislist)
#' require(data.table)
#' SNOMED <- sampleSNOMED()
#'
#' actual = data.table(id = c(2, 2, 3, 3, 3),
#'   conceptId = as.SNOMEDconcept(c(
#'   'Diastolic heart failure', 
#'   'Systolic heart failure', 
#'   'Systolic dysfunction', 
#'   'Acute heart failure',
#'   'Cardiac finding')))
#' # first two are correct, third concept is vaguer than gold standard
#' # (i.e. true in 'relaxed' mode but not if relaxed = FALSE),
#' # fourth concept is a false positive and fifth is an ancestor
#' # which should be ignored
#'
#' goldstandard = data.table(id = c(1, 2, 2, 2, 3, 3),
#'   conceptId = c('703276005', '78862003', '417996009|703273002',
#'   '443253003', '56675007', '84114007'))
#' # first and second are not present in actual (i.e. false negatives),
#' # third has two correct options, last is an ancestor of another
#' # term so it should be ignored
#'
#' evaluateNER(actual, goldstandard)
#' evaluateNER(actual, goldstandard, relaxed = TRUE)
#' evaluateNER(actual, goldstandard, aggregate = FALSE)
evaluateNER <- function(actual, goldstandard, relaxed = FALSE,
	aggregate = TRUE, SNOMED = Rdiagnosislist::getSNOMED(),
	subset = NULL){
	
	# Declare column names for CRAN check
	conceptId <- id <- found <- linked <- NULL
	truepos <- falsepos <- falseneg <- F1 <- NULL
	precision <- recall <- NULL
	
	# Prepare actual annotations for evaluation
	A <- as.data.table(copy(actual))
	if (!(all(c('id', 'conceptId') %in% names(A)))){
		stop('actual must contain columns id and conceptId')
	}
	if (!is.null(subset)){
		subset <- as.SNOMEDconcept(subset, SNOMED = SNOMED)
	}
	if (!is.null(subset)){
		A <- A[conceptId %in% subset]
	}
	A <- A[!duplicated(A)][, list(
		conceptId = remove_ancestors(conceptId,
			SNOMED = SNOMED)), by = id]
	
	# Prepare gold standard for recall
	G_recall <- as.data.table(copy(goldstandard))
	if (!(all(c('id', 'conceptId') %in% names(G_recall)))){
		stop('goldstandard must contain columns id and conceptId')
	}
	G_recall[, conceptId := as.character(conceptId)]
	# Ensure that conceptId fields with multiple entries have a
	# consistent order to enable deduplication
	G_recall[, conceptId := sapply(strsplit(conceptId, '\\|'),
		function(x) paste(sort(unique(x)), collapse = '|'))]
	G_recall <- G_recall[!duplicated(G_recall)]
	# Now convert to SNOMEDconcept (integer64)
	G_recall[, conceptId := lapply(strsplit(conceptId, '\\|'),
		function(x) as.SNOMEDconcept(x, SNOMED = SNOMED))]
	if (!is.null(subset)){
		G_recall[, conceptId := lapply(conceptId, function(x)
			intersect(x, subset))]
	}
	# G_recall is a table of id with each linked concept separately
	ids <- unique(G_recall$id)
	
	# Prepare gold standard for precision
	G_precision <- data.table(id = ids, valid_conceptId = lapply(ids,
		function(x){ remove_ancestors(
			as.SNOMEDconcept(unlist(sapply(G_recall[id == x]$conceptId,
			as.character)), SNOMED = SNOMED), SNOMED = SNOMED)
		}))
	# G_precision is a table of id with all linked concepts
	# Remove unnecessary concepts from G_recall
	G_recall <- merge(G_recall, G_precision, by = 'id')
	G_recall[, conceptId := lapply(1:.N, function(i){
			intersect(G_recall[i]$conceptId[[1]],
				G_recall[i]$valid_conceptId[[1]])
		})]
	G_recall <- G_recall[sapply(conceptId, function(x) length(x) > 0)]
	# Now use A, G_recall and G_precision for checking
	G_recall[, found := NA]
	A[, linked := NA]
	
	E <- rbindlist(lapply(ids, function(x){
		if (any(A$id == x)){
			if (relaxed){
				G_recall[id == x, found := sapply(conceptId, function(i){
					any(i %in% descendants(A[id == x]$conceptId,
						SNOMED = SNOMED, include_self = TRUE))
				})]
				A[id == x, linked := conceptId %in%
					ancestors(G_precision[id == x]$valid_conceptId[[1]],
					SNOMED = SNOMED, include_self = TRUE)]
			} else {
				G_recall[id == x, found := sapply(conceptId, function(i){
					any(i %in% A[id == x]$conceptId)
				})]
				A[id == x, linked := conceptId %in% as.SNOMEDconcept(
					G_precision[id == x]$valid_conceptId[[1]],
					SNOMED = SNOMED)]
			}
		} else {
			G_recall[id == x, found := FALSE]
		}
		OUT <- data.table(
			truepos = sum(G_recall[id == x]$found),
			falseneg = sum(!(G_recall[id == x]$found)),
			falsepos = sum(!(A[id == x]$linked)))
		OUT[, precision := truepos / (truepos + falsepos)]
		OUT[, recall := truepos / (truepos + falseneg)]
		OUT[, F1 := 2 / (1 / precision + 1 / recall)]
		OUT
	}))
	
	if (aggregate){
		OUT <- E[, list(
			truepos = sum(truepos),
			falseneg = sum(falseneg),
			falsepos = sum(falsepos))]
		OUT[, precision := truepos / (truepos + falsepos)]
		OUT[, recall := truepos / (truepos + falseneg)]
		OUT[, F1 := 2 / (1 / precision + 1 / recall)]
		return(OUT)
	} else {
		return(E)
	}
}
