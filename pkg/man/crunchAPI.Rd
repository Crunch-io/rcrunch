\name{crunchAPI}
\alias{crunchAPI}
\title{Main Crunch API handling function}
\usage{
  crunchAPI(http.verb, url,
    response.handler = handleAPIresponse,
    config = list(verbose = FALSE),
    status.handlers = list(), ...)
}
\arguments{
  \item{http.verb}{character in GET, PUT, POST}

  \item{url}{character URL to do the verb on}

  \item{...}{additional arguments}

  \item{response.handler}{function that takes a http
  response object and does something with it}

  \item{config}{list of config parameters. See httr
  documentation.}
}
\description{
  Main Crunch API handling function
}

