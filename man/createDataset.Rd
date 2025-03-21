% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/new-dataset.R
\name{createDataset}
\alias{createDataset}
\title{Create an empty dataset}
\usage{
createDataset(name, body, ..., project = defaultCrunchProject())
}
\arguments{
\item{name}{character, the name to give the new Crunch dataset. This is
required.}

\item{body}{list correctly formatted metadata definition for a dataset. See
the \href{https://crunch.io/api/reference/#post-/datasets/}{Crunch API documentation}.}

\item{...}{additional arguments for the POST to create the dataset, such as
"description".}

\item{project}{A \code{ProjectFolder} object, string path that could be passed to \code{\link[=cd]{cd()}}
relative to the root project, or a URL for a \code{ProjectFolder}. If left empty,
rcrunch will look in \code{envOrOption('crunch.default.project')} and error if nothing
is found.}
}
\value{
An object of class CrunchDataset.
}
\description{
Use only if you're writing a function to create a Crunch dataset from a
custom data structure. If you have a \code{data.frame}, just call
\code{\link[=newDataset]{newDataset()}} on it.
}
\seealso{
\code{\link[=newDataset]{newDataset()}}
}
\keyword{internal}
