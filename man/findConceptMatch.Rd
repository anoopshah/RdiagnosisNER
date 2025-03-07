% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{findConceptMatch}
\alias{findConceptMatch}
\title{Find SNOMED CT concepts matching a portion of text}
\usage{
findConceptMatch(text, lemma = text, CDB, SNOMED)
}
\arguments{
\item{text}{text phrase to match}

\item{lemma}{lemma to match}

\item{CDB}{concept database environment}

\item{SNOMED}{SNOMED CT dictionary environment}
}
\value{
unique SNOMEDconcept vector of matches
}
\description{
This is a simple dictionary-based named entity recognition (NER)
function. If there are multiple concepts with the same term,
this function may return multiple matches.
}
\seealso{
showparse, parseSentence, addLemmaToCDB
}
