\name{crunchAPI}
\alias{crunchAPI}
\title{Main Crunch API handling function}
\usage{
  crunchAPI(http.verb, url,
    response.handler = handleAPIresponse, config = list(),
    status.handlers = list(), ...)
}
\arguments{
  \item{http.verb}{character in GET, PUT, POST}

  \item{url}{character URL to do the verb on}

  \item{...}{additional arguments passed to \code{GET},
  \code{PUT}, or \code{POST}}

  \item{response.handler}{function that takes a http
  response object and does something with it}

  \item{config}{list of config parameters. See httr
  documentation.}

  \item{status.handlers}{named list of specific HTTP
  statuses and a response function to call in the case
  where that status is returned. Passed to the
  \code{response.handler} function.}
}
\description{
  Main Crunch API handling function
}

