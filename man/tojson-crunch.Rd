% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/json.R
\name{tojson-crunch}
\alias{tojson-crunch}
\alias{jsonprep}
\alias{jsonprep,AbstractCategories-method}
\alias{jsonprep,ANY-method}
\alias{jsonprep,ShojiOrder-method}
\alias{jsonprep,OrderGroup-method}
\alias{toJSON}
\title{toJSON methods for Crunch objects}
\usage{
jsonprep(x, ...)

\S4method{jsonprep}{AbstractCategories}(x, ...)

\S4method{jsonprep}{ANY}(x, ...)

\S4method{jsonprep}{ShojiOrder}(x, ...)

\S4method{jsonprep}{OrderGroup}(x, ...)

toJSON(x, ..., for_query_string = FALSE)
}
\arguments{
\item{x}{the object}

\item{...}{additional arguments}

\item{for_query_string}{If \code{TRUE}, and \code{crunch.stabilize.query} option
is also set to \code{TRUE}, then dictionary items in the JSON are
sorted alphabetically, which can be useful when capturing
mocks using "httptest".}
}
\value{
\code{jsonprep} returns a base R object that \code{jsonlite::toJSON}
can handle. \code{toJSON} returns the JSON-serialized character object.
}
\description{
\code{crunch} uses the \code{jsonlite} package for JSON serialization and
deserialization. Unfortunately, \code{\link[jsonlite:fromJSON]{jsonlite::toJSON()}}
does not allow for defining S4 methods for other object types. So,
\code{crunch::toJSON} wraps \code{jsonprep}, which exists to translate
objects to base R objects, which \code{jsonlite::toJSON} can handle.
\code{jsonprep} is defined as an S4 generic, and it is exported, so you can define
methods for it if you have other
objects that you want to successfully serialize to JSON.
}
\seealso{
\code{\link[jsonlite:fromJSON]{jsonlite::toJSON()}}
}
