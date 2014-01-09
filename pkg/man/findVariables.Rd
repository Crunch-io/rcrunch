\name{findVariables}
\alias{findVariables}
\title{Search a Dataset or list of Variables}
\usage{
  findVariables(dataset, pattern, key = "alias", ...)
}
\arguments{
  \item{dataset}{the Dataset or list of Crunch objects to
  search}

  \item{pattern}{regular expression, passed to \code{grep}}

  \item{key}{the field in the Crunch objects in which to
  grep}

  \item{...}{additional arguments passed to \code{grep}}
}
\value{
  indices of the Variables that match the pattern, or the
  matching key values if value=TRUE is passed to
  \code{grep}
}
\description{
  A version of \code{\link{grep}} for Crunch objects
}

