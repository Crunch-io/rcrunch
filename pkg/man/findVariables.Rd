\name{findVariables}
\alias{findVariables}
\title{Search a Dataset or list of Variables}
\usage{
  findVariables(dataset, pattern = "",
    key = namekey(dataset), ...)
}
\arguments{
  \item{dataset}{the Dataset or list of Crunch objects to
  search}

  \item{pattern}{regular expression, passed to \code{grep}.
  If "", returns all.}

  \item{key}{the field in the Crunch objects in which to
  grep}

  \item{...}{additional arguments passed to \code{grep}. If
  \code{value=TRUE}, returns the values of \code{key} where
  matches are found, not the variables themselves}
}
\value{
  indices of the Variables that match the pattern, or the
  matching key values if value=TRUE is passed to
  \code{grep}
}
\description{
  A version of \code{\link{grep}} for Crunch objects
}

