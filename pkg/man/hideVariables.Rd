\name{hideVariables}
\alias{hiddenVariables<-}
\alias{hideVariables}
\alias{unhideVariables}
\title{Hide and Unhide Variables Within a Dataset}
\usage{
  hideVariables(dataset, variables, pattern = NULL,
    key = "alias", ...)

  hiddenVariables(x, value) <- value

  unhideVariables(dataset, variables, pattern = NULL,
    key = "alias", ...)
}
\arguments{
  \item{dataset}{the Dataset to modify}

  \item{x}{same as \code{dataset}, for `hiddenVariables<-`}

  \item{variables}{names or indices of variables to
  (un)hide}

  \item{value}{same as \code{variables}, for
  `hiddenVariables<-`}

  \item{pattern}{optional regular expression to identify
  Variables to (un)hide}

  \item{key}{the Variable attribute to \code{\link{grep}}
  with the \code{pattern}. Default is "alias"}

  \item{...}{optional additional arguments to \code{grep}}
}
\value{
  (invisibly) \code{dataset} with the specified variables
  (un)hidden
}
\description{
  Hide and Unhide Variables Within a Dataset
}

