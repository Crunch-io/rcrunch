#' Search Crunch for datasets.
#'
#' @param query the text to search for in datasets and their variables
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
