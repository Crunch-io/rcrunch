\name{grouped}
\alias{grouped}
\alias{ungrouped}
\title{Get un(grouped) VariableGroups}
\usage{
  grouped(var.order)

  ungrouped(var.order)
}
\arguments{
  \item{var.order}{an object of class VariableOrder}
}
\value{
  For grouped(), a VariableOrder with "ungrouped" omitted.
  For ungrouped(), a VariableGroup.
}
\description{
  "ungrouped" is a magic VariableGroup that contains all
  variables not found in any of the other groups.
  Regardless of whether you create it, the server will
  return it. You can modify the order of the variables
  within it, but which variables are present in it is
  determined server-side.
}

