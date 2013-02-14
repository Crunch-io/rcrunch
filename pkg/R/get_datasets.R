##' Show the names of all Crunch datasets
##' @param refresh logical: should the function check the Crunch API for new datasets? Default is FALSE. 
##' @return Character vector of dataset names, each of which would be a valid input for \code{\link{loadDataset}}
##' @export 
listDatasets <- function (refresh=FALSE) {
    if (refresh && crunchAPIcanBeReached()) {
        updateDatasetList()
    }
    return(names(dataset_list()))
}

##' Refresh the local list of Crunch datasets
##' @param x the dataset URLs. You shouldn't change the default.
##' @return Nothing. Called for its side effects of setting local environment variables.
##' @export 
updateDatasetList <- function (x=getUserDatasetURLs()) {
    saveDatasetURLs(x)
    updateDatasetListFromURLs()
}

getUserDatasetURLs <- function () {
    getShojiCollectionURLs(sessionURL("datasets_url"))
}

saveDatasetURLs <- function (x=getUserDatasetURLs()) {
    saveSessionURLs(list(user_dataset_urls=x))
}

datasetsAreStale <- function (crunch.datasets=getUserDatasetURLs()) {
    length(session_store$datasets) != length(crunch.datasets)
}

updateDatasetListFromURLs <- function (x=sessionURL("user_dataset_urls")) {
    session_store$datasets <- getDatasetObjects(x)
}

dataset_list <- function () session_store$datasets

getDatasetObjects <- function (x=sessionURL("user_dataset_urls")) {
    getShojiCollectionContents(x, "body$name") ## or alias?
}

##' Load a Crunch Dataset
##' @param dataset.name character, the name of a Crunch dataset you have access to. 
##' @param dataset.list, the local session store from which to retrieve the dataset's resource URLs. You shouldn't change this.
##' @return An object of class \code{CrunchDataset}
##' @export 
loadDataset <- function (dataset.name, dataset.list=dataset_list()) {
    dataset <- selectDatasetFromList(dataset.name, dataset.list)
    return(as.dataset(dataset))
}

selectDatasetFromList <- function (name, dslist=NULL) {
    this.dataset <- dslist
    if (!is.null(this.dataset)) {
        this.dataset <- this.dataset[[name]]
    }
    if (is.null(this.dataset)) {
        stop(paste(name, "not found"), call.=FALSE)
    }
    return(this.dataset)
}
