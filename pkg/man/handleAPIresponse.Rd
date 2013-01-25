\name{handleAPIresponse}
\alias{handleAPIresponse}
\title{Do the right thing with the HTTP response}
\usage{
  handleAPIresponse(response, special.statuses = list())
}
\arguments{
  \item{response}{an httr response object}

  \item{special.statuses}{an optional named list of
  functions by status code.}
}
\value{
  The full HTTP response object, just the content, or any
  other status-specific action
}
\description{
  Do the right thing with the HTTP response
}

