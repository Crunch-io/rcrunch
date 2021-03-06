% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cube-subset.R
\name{skipMissingCategories}
\alias{skipMissingCategories}
\title{Handle missing categories in CrunchCube}
\usage{
skipMissingCategories(cube, index, drop)
}
\arguments{
\item{cube}{a CrunchCube}

\item{index}{the user supplied index}

\item{drop}{whether dimensions should be dropped}
}
\value{
A list translated indexes
}
\description{
Missing categories are not displayed when \code{cube@useNA} is set to \code{"no"} or \code{ifANy}.
The way we handle these cases is to change the \code{useNA} setting to \code{always}, subset
the cube, and then change it back to the original value. For this to work we
need to translate the indices that the user supplied to properly skip over hidden
categories.
}
\keyword{internal}
