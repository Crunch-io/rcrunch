getUserDatasetURLs <- function () {
    GET(sessionURL("datasets_url"))$entities
}

saveDatasetURLs <- function (x=getUserDatasetURLs()) {
    saveSessionURLs(list(user_dataset_urls=x))
}

##' @export 
listDatasets <- function (refresh=FALSE) {
    latest.datasets <- getUserDatasetURLs()
    if (refresh || datasetsAreStale(latest.datasets)) {
        saveDatasetURLs(latest.datasets)
        updateDatasetList()
    }
    return(names(session_store$datasets))
}

datasetsAreStale <- function (crunch.datasets=getUserDatasetURLs()) {
    length(session_store$datasets) != length(crunch.datasets)
}

##' @export 
updateDatasetList <- function () {
    session_store$datasets <- lapply(session_store$user_dataset_urls,
        GET)
    names(session_store$datasets) <- selectFrom("name", session_store$datasets) ## or alias?
}

##' @export 
loadFromCrunch <- function (dataset.name) {
    this.dataset <- session_store$datasets
    if (is.null(this.dataset)) {
        stop(paste(dataset.name, "not found"))
    }
    ## GET variables
    ## S3 class it
    return(this.dataset)
}
