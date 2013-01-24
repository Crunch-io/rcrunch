\name{listDatasets}
\alias{listDatasets}
\title{Show the names of all Crunch datasets}
\usage{
  listDatasets(refresh = FALSE)
}
\arguments{
  \item{refresh}{logical: should the function check the
  Crunch API for new datasets? Default is FALSE.}
}
\value{
  Character vector of dataset names, each of which would be
  a valid input for \code{\link{loadDataset}}
}
\description{
  Show the names of all Crunch datasets
}

