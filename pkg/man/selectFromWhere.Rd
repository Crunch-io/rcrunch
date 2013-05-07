\name{selectFromWhere}
\alias{selectFromWhere}
\title{Extract list element(s) conditionally}
\usage{
  selectFromWhere(where = TRUE, xlist, key = NULL,
    ifnot = NA, simplify = TRUE)
}
\arguments{
  \item{where}{an expression to be evaluated in each
  element in \code{xlist}. This determines which list
  objects to keep. The expression must return a logical.}

  \item{key}{character naming the key(s) to extract.
  Default is \code{NULL}, meaning that the entire list
  element is to be kept.}

  \item{xlist}{list containing other lists from which you
  want to extract. Passed to \code{\link{selectFrom}}.}

  \item{ifnot}{what to return if the key is not found in a
  given xlist element. Passed to \code{\link{selectFrom}}.}

  \item{simplify}{logical, passed to
  \code{\link{selectFrom}}.}
}
\description{
  Extract list element(s) conditionally
}
\examples{
vars <- loadJSONMocks("variables.json")
selectFromWhere(name=="Gender", vars)
}

