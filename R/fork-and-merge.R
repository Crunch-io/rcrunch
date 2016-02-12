forks <- function (dataset) {
    stopifnot(is.dataset(dataset))
    return(ForkCatalog(crGET(shojiURL(dataset, "catalogs", "forks"))))
}

setMethod("names", "ForkCatalog",  function (x) getIndexSlot(x, "name"))

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
