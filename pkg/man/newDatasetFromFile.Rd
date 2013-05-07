\name{newDatasetFromFile}
\alias{newDatasetFromFile}
\title{Upload a file to Crunch to make a new dataset}
\usage{
  newDatasetFromFile(file, name = basename(file), ...)
}
\arguments{
  \item{file}{character, the path to a file to upload}

  \item{name}{character, the name to give the new Crunch
  dataset. Default is the file name}

  \item{...}{additional arguments, currently not
  implemented}
}
\value{
  If successful, an object of class crunchdf.
}
\description{
  Upload a file to Crunch to make a new dataset
}

