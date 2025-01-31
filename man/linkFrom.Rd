% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/link.R
\name{linkFrom}
\alias{linkFrom}
\alias{linkTo}
\title{Find linked concepts}
\usage{
linkFrom(D, i, from = "ANY", lemma_regex = ".*", to = "ANY")

linkTo(D, i, to = "ANY", lemma_regex = ".*", from = "ANY")
}
\arguments{
\item{D}{spacy parse table containing columns: token, lemma, dep_rel,
head, semType}

\item{i}{vector of indices of base word(s)}

\item{from}{vector of dependency relations to map from, or "ANY"}

\item{lemma_regex}{regular expression condition for lemma}

\item{to}{vector of dependency relations to map to, or "ANY"}
}
\value{
integer vector of indices of linked words, or a zero length
  vector if no link
}
\description{
Finds concepts semantically related to the index concept. These
functions are called by addAnd, findAllergy, findAttr etc.
}
\examples{
D <- showparse("ulcers of the left big and second toes due to diabetes")
}
\seealso{
addAnd, findAllergy, findAttr, findBody, findCause,
  findLaterality, linkFrom, linkTo

Other linkFunction: 
\code{\link{addAnd}()},
\code{\link{findAllergy}()}

Other linkFunction: 
\code{\link{addAnd}()},
\code{\link{findAllergy}()}
}
\concept{linkFunction}
