searchDatasets <- function (query, ...) {
    search_url <- shojiURL(datasets(), "views", "search")
    results <- SearchResults(crGET(search_url, query=list(q=query, ...)))
    ## Grab useful things out of the (odd) API response
    return(SearchResults(results[["groups"]][[1]]))
}

#' @rdname dataset-variables
#' @export
setMethod("variables", "SearchResults", function (x) {
    ## Close enough to a catalog object
    VariableCatalog(structure(list(index=x$variables), class="shoji"))
})
