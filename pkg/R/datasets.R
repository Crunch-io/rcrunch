getUserDatasetURLs <- function () {
    crunchAPI("GET", sessionURL("datasets_url"))$entities
}

saveDatasetURLs <- function (x=getUserDatasetURLs()) {
    saveSessionURLs(list(user_dataset_urls=x))
}

##' @export 
listDatasets <- function (refresh=FALSE) {
    if (refresh || datasetsAreStale()) updateDatasetList()
    return(names(session_store$datasets))
}

datasetsAreStale <- function () {
    length(session_store$datasets) != length(session_store$user_dataset_urls)
}

##' @export 
updateDatasetList <- function () {
    session_store$datasets <- lapply(session_store$user_dataset_urls,
        fetch_a_dataset)
    names(session_store$datasets) <- selectFrom(session_store$datasets, 
        "alias") ## or just name?
}

fetch_a_dataset <- function (ds.url) {
    out <- crunch.API("GET", ds.url)
    ## S3 class this?
    return(out)
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

