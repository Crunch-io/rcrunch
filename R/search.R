#' Search Crunch for datasets.
#'
#' `searchDatasets` searches datasets' metadata for matches to the `query`
#' argument. This search will include variable names, aliases, categories, but not the content
#' of text variables. See [the API Documentation](http://docs.crunch.io/#search) for more information
#' about searching Crunch.
#'
#' @param query the text to search for in datasets and their variables (note:
#' only alpha characters will be used, numbers and other characters will be discarded.)
#' @param ... additional options provided to the search endpoint.
#' @return If successful, an object of class SearchResults
#' @export
searchDatasets <- function (query, ...) {
    search_url <- sessionURL("search", "views")
    results <- SearchResults(crGET(search_url, query=list(q=query, grouping="datasets", ...)))
    ## Grab useful things out of the (odd) API response
    return(SearchResults(results[["groups"]][[1]]))
}

#' @rdname dataset-variables
#' @export
setMethod("variables", "SearchResults", function (x) {
    ## Close enough to a catalog object
    VariableCatalog(structure(list(index=x$variables), class="shoji"))
})
