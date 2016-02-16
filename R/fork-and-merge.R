forks <- function (dataset) {
    stopifnot(is.dataset(dataset))
    return(ForkCatalog(crGET(shojiURL(dataset, "catalogs", "forks"))))
}

##' @rdname describe-catalog
##' @export
setMethod("names", "ForkCatalog",  function (x) getIndexSlot(x, "name"))

##' Create a fork of a dataset
##'
##' As with many other version control systems, in Crunch you can fork a
##' dataset's revision history, effectively making a copy on which you can work
##' independently of the original dataset. You can then merge those change back
##' to the original dataset or keep working independently.
##' @param dataset The CrunchDataset to fork
##' @param forkname character name to give the fork. If omitted, one will be
##' provided for you
##' @return The new fork, a CrunchDataset.
##' @export
forkDataset <- function (dataset, forkname) {
    if (missing(forkname)) {
        nforks <- length(forks(dataset))
        prefix <- ifelse(nforks, paste0("Fork #", nforks + 1, " of"), "Fork of")
        forkname <- paste(prefix, name(dataset))
    }
    fork_url <- crPOST(shojiURL(dataset, "catalogs", "forks"),
        body=toJSON(list(element="shoji:entity", body=list(name=forkname))))
    updateDatasetList()
    return(entity(datasetCatalog()[[fork_url]]))
}

mergeFork <- function (dataset, fork, autorollback=TRUE) {
    m <- crPOST(shojiURL(dataset, "catalogs", "actions"), body=toJSON(list(
        element="shoji:entity",
        body=list(dataset=self(fork), autorollback=autorollback)
    )))
    return(refresh(dataset))
}
