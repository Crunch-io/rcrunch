\name{newDataset}
\alias{newDataset}
\title{Upload a data.frame to Crunch to make a new dataset}
\usage{
  newDataset(x, name = substitute(x),
    useAlias = default.useAlias(), ...)
}
\arguments{
  \item{x}{a data.frame or other rectangular R object}

  \item{name}{character, the name to give the new Crunch
  dataset. Default is the name of the R object passed in
  \code{x}}

  \item{...}{additional arguments passed to \code{
  \link{newDatasetFromFile}}}
}
\value{
  If successful, an object of class CrunchDataset.
}
\description{
  Upload a data.frame to Crunch to make a new dataset
}

