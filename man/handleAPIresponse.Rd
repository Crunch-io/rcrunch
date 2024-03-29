% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api.R
\name{handleAPIresponse}
\alias{handleAPIresponse}
\title{Do the right thing with the HTTP response}
\usage{
handleAPIresponse(response, special.statuses = list(), progress.handler = NULL)
}
\arguments{
\item{response}{an httr response object}

\item{special.statuses}{an optional named list of functions by status code.}

\item{progress.handler}{an optional function to handle errors reported by
a progress result. Default NULL prints the string \code{message}; other
functions required to handle non-string messages in progress responses.}
}
\value{
The full HTTP response object, just the content, or any other
status-specific action
}
\description{
Do the right thing with the HTTP response
}
\keyword{internal}
