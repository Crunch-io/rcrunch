\name{crunchAPI}
\alias{crunchAPI}
\title{Main Crunch API handling function}
\usage{
  crunchAPI(http.verb, url, ..., auth.required = TRUE)
}
\arguments{
  \item{http.verb}{character in GET, PUT, POST}

  \item{url}{character URL to do the verb on}

  \item{...}{additional arguments}

  \item{auth.required}{logical: does this API call require
  authentication? If so, the function will try to fetch the
  session token from the local store, and it will throw an
  error if no session has been authenticated}
}
\description{
  Main Crunch API handling function
}

