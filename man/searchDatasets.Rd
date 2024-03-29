% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/search.R
\name{searchDatasets}
\alias{searchDatasets}
\title{Search Crunch for datasets.}
\usage{
searchDatasets(query, f = NULL, ...)
}
\arguments{
\item{query}{the text to search for in datasets and their variables (note:
only alpha characters will be used, numbers and other characters will be discarded.)}

\item{f}{A list of filter parameters, see the filter parameters of
\href{https://crunch.io/api/reference/#get-/search/}{the API Documentation}
for more details.}

\item{...}{additional options provided to the search endpoint.}
}
\value{
If successful, an object of class SearchResults
}
\description{
\code{searchDatasets} searches datasets' metadata for matches to the \code{query}
argument. This search will include variable names, aliases, categories, but not the content
of text variables. See \href{https://crunch.io/api/reference/#get-/search/}{the API Documentation} for
more information about searching Crunch.
}
