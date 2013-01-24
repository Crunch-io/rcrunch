\name{newDataset}
\alias{newDataset}
\title{Upload a data.frame to Crunch to make a new dataset}
\usage{
  newDataset(x, name = substitute(x), ...)
}
\arguments{
  \item{x}{a data.frame or other rectangular R object}

  \item{name}{character, the name to give the new Crunch
  dataset. Default is the name of the R object passed in
  \code{x}}
}
\value{
  If successful, an object of class crunchdf.
}
\description{
  Upload a data.frame to Crunch to make a new dataset
}

