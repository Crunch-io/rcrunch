#' Search Crunch for datasets.
#'
#' `searchDatasets` searches datasets' metadata for matches to the `query`
#' argument. This search will include variable names, aliases, categories, but not the content
#' of text variables. See [the API Documentation](https://crunch.io/api/reference/#get-/search/) for
#' more information about searching Crunch.
#'
#' @param query the text to search for in datasets and their variables (note:
#' only alpha characters will be used, numbers and other characters will be discarded.)
#' @param f A list of filter parameters, see the filter parameters of
#' [the API Documentation](https://crunch.io/api/reference/#get-/search/)
#' for more details.
#' @param ... additional options provided to the search endpoint.
#' @return If successful, an object of class SearchResults
#' @export
searchDatasets <- function(query, f = NULL, ...) {
    if (!is.character(query)) {
        halt("Search query must be a string, not ", class(query))
    }
    if (length(query) != 1) {
        halt(
            "Search query must be a single string, not a length-",
            length(query), " character vector"
        )
    }

    search_url <- sessionURL("search", "views")
    ## TODO: should this GET be uncached()? Every edit anywhere is going to
    ## require dropping cache
    url_query <- list(q = query, grouping = "datasets", ...)
    if (!is.null(f)) url_query$f <- toJSON(f, for_query_string = TRUE)
    results <- SearchResults(crGET(search_url, query = url_query))
    ## Grab useful things out of the (odd) API response
    return(SearchResults(results[["groups"]][[1]]))
}

#' @importFrom curl curl_escape
findDatasetsByName <- function(x) {
    u <- paste0(sessionURL("datasets"), "by_name/", curl_escape(x), "/")
    out <- DatasetCatalog(crGET(u))
    ## HACK: set the self to be the datasets root catalog so we don't try to
    ## PATCH the wrong thing off of this
    out@self <- sessionURL("datasets")
    return(out)
}
