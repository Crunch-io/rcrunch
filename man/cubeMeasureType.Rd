% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cube-result.R
\name{cubeMeasureType}
\alias{cubeMeasureType}
\alias{cubeMeasureType,CrunchCube-method}
\title{Get measure type of cube result}
\usage{
cubeMeasureType(x, measure = NULL)

\S4method{cubeMeasureType}{CrunchCube}(x, measure = 1)
}
\arguments{
\item{x}{A \code{CrunchCube}}

\item{measure}{Which measure in the cube to check, can index by position
with numbers or by name. NULL, the default, will select a "sum" type measure
first, "mean" if no sum is available, and will use the cube's names in alphabetic
order if there are no "sum" or "mean" measures (or if a tie breaker between two
measure types is needed).}
}
\value{
A string describing the cube's measure type
}
\description{
Returns a string describing the measure type of the cube result,
such as "count", "mean", "sd", etc.
}
\examples{
\dontrun{
cube1 <- crtabs(~allpets, ds)
cubeMeasureType(cube1)
#> "count"

cube2 <- crtabs(list(a = n(), b = mean(age)) ~ allpets, ds)
cubeMeasureType(cube2)
#> "count"
cubeMeasureType(cube2, "b")
#> "mean"
}
}
