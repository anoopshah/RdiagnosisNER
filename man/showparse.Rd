% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{showparse}
\alias{showparse}
\title{Parse a phrase using spacy parse}
\usage{
showparse(x, min_len_normalize = 5)
}
\arguments{
\item{x}{text to process with spacy parse}

\item{min_len_normalize}{tokenswith fewer than this number of
characters will not be lemmatised}
}
\value{
data.table with columns token, lemma, dep_rel
  (dependency relation) and head (index number of token that the
  dependency relates to)
}
\description{
Applies the 'spacy parse' function to a phrase.
}
\examples{
D <- showparse("fracture of left femur and right humerus")
D
}
\seealso{
parseSentence
}
