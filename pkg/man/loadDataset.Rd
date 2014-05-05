\name{loadDataset}
\alias{loadDataset}
\title{Load a Crunch Dataset}
\usage{
  loadDataset(dataset.name,
    kind = c("active", "all", "archived"),
    useAlias = default.useAlias())
}
\arguments{
  \item{dataset.name}{character, the name of a Crunch
  dataset you have access to.}

  \item{dataset.list,}{the local session store from which
  to retrieve the dataset's resource URLs. You shouldn't
  change this.}

  \item{useAlias}{logical whether variable alias or name
  should be used as R variable names when the dataset is
  returned. Default is TRUE, meaning alias. They're more
  computer friendly.}
}
\value{
  An object of class \code{CrunchDataset}
}
\description{
  Load a Crunch Dataset
}

