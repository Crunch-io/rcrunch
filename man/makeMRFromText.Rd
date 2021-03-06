% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make-array.R
\name{makeMRFromText}
\alias{makeMRFromText}
\title{Create Multiple Response Variable from Delimited lists}
\usage{
makeMRFromText(
  var,
  delim,
  name,
  selected = "selected",
  not_selected = "not_selected",
  unanswered = NA,
  ...
)
}
\arguments{
\item{var}{The variable containing the delimited responses}

\item{delim}{The delimiter separating the responses}

\item{name}{The name of the resulting MR variable}

\item{selected}{A character string used to indicate a selection, defaults to
"selected"}

\item{not_selected}{Character string identifying non-selection, defaults to
"not_selected"}

\item{unanswered}{Character string indicating non-response, defaults to NA.}

\item{...}{Other arguments to be passed on to \code{\link[=makeMR]{makeMR()}}}
}
\value{
a Multiple response variable definition
}
\description{
Surveys often record multiple response questions in delimited lists where
each respondent's selections are separated by a delimiter like \verb{;} or \code{|}.
This function breaks the delimited responses into subvariables, uploads those
subvariables to Crunch, and finally creates a multiple response variable from
them.
}
