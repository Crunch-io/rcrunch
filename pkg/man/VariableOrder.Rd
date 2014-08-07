\name{VariableOrder}
\alias{VariableGroup}
\alias{VariableOrder}
\alias{ordering}
\alias{ordering<-}
\title{Organize Variables within a Dataset}
\usage{
  VariableOrder(...)

  VariableGroup(...)
}
\description{
  Variables in the Crunch web application can be viewed in
  an ordered, hierarchical list. These objects and methods
  allow you to modify that order from R.
}
\details{
  A VariableOrder object is a subclass of \code{list} that
  contains VariableGroups. VariableGroup objects contain a
  group name and an set of "entities", which can be
  variable references or other nested VariableGroups.

  The \code{ordering} methods allow you to get and set a
  VariableOrder on a \code{\link{CrunchDataset}} or on the
  \code{\link{VariableCatalog}} that the dataset contains.
}

