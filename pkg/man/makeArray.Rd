\name{makeArray}
\alias{makeArray}
\alias{makeMR}
\title{Make a Categorical Array or Multiple Response variable}
\usage{
  makeArray(list_of_variables, dataset = NULL,
    pattern = NULL, key = "alias", name, ...)

  makeMR(list_of_variables, dataset = NULL, pattern = NULL,
    key = "alias", name, selections, ...)
}
\arguments{
  \item{list_of_variables}{a list of Variable objects to
  bind together, or a Dataset object containing only the
  Variables to bind (as in from subsetting a Dataset). If
  omitted, must supply \code{dataset} and \code{pattern}.}

  \item{dataset}{the Crunch Dataset to which the variables
  in \code{list_of_variables} belong, or in which to search
  for variables based on \code{pattern}. If omitted,
  \code{list_of_variables} must exist and all Variables in
  the list must belong to the same Dataset}

  \item{pattern}{An optional regular expression to search
  for variables to bind within \code{dataset}.}

  \item{key}{character, the name of the Variable field in
  which to search with \code{pattern}. Default is 'alias'.}

  \item{name}{character, the name that the new Categorical
  Array variable should have. Required.}

  \item{selections}{character, for \code{makeMR}, the names
  of the categories to mark as the dichotomous selections.
  Required for \code{makeMR}; ignored in \code{makeArray}.}

  \item{...}{Optional additional attributes to set on the
  new variable. Not yet supported.}
}
\value{
  The object of class CategoricalArrayVariable or
  MultipleResponseVariable corresponding to the
  just-created variable on the server.
}
\description{
  Make a Categorical Array or Multiple Response variable
}

