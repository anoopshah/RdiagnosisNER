#' RdiagnosisNER: A package for named entity recognition for SNOMED CT
#' diagnoses
#'
#' This package works together with Rdiagnosislist to provide rule-based
#' named entity recognition functionality including the ability to
#' link basic concepts with attributes to more specific SNOMED CT
#' concepts using the 'compose' function.
#'
#' To use this package, first it is necessary to load a SNOMED
#' dictionary and create a 'concept database' (CDB) lookup using the
#' 'loadSNOMED' and 'createCDB' functions in the Rdiagnosislist
#' package. It is then necessary to lemmatize the lookup terms using
#' 'addLemmaToCDB'.
#'
#' To use the SNOMED CT composition feature, it is then necessary to
#' create a composition lookup using the 'decompose' function in
#' Rdiagnosislist, and add it to the CDB using 'addComposeLookupToCDB'.
#'
#' You can then use the named entity recognition tool by calling the
#' relevant function:
#'
#' \describe{
#'   \item{NERsentence}{ to analyse a single sentence, returns the
#'     annotated text and the extracted findings.} 
#'   \item{NERdocument}{ splits a document into sentences, analyses
#'     each sentence separately using NERsentence, and returns the
#'     annotated text and extracted findings.}
#'   \item{NERcorpus}{ analyses a set of documents using NERdocument,
#'     and returns the extracted findings.}
#' }
#'
#' The algorithm is mostly rule-based, apart from the spacy neural
#' network based dependency parser which is used to derive links between
#' words and phrases (e.g. linking a diagnosis to its attributes).
#'
#' The algorithm does not attempt to detect the context of term mentions
#' (e.g. negation), which would need to be handled separately.
#'
#' @seealso addLemmaToCDB NERsentence
#' @keywords internal 
"_PACKAGE"
