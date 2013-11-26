##' Show the names of all Crunch datasets
##' @param refresh logical: should the function check the Crunch API for new datasets? Default is FALSE. 
##' @return Character vector of dataset names, each of which would be a valid input for \code{\link{loadDataset}}
##' @export 
listDatasets <- function (refresh=FALSE) {
    if (refresh && crunchAPIcanBeReached()) {
        updateDatasetList()
    }
    return(vapply(dataset_collection(), function (x) x$datasetName, character(1)))
}

##' Refresh the local list of Crunch datasets
##' @return Nothing. Called for its side effects of setting local environment variables.
##' @export 
updateDatasetList <- function () {
    session_store$datasets <- getDatasetCollection()
}

## Get the dataset collection from the appropriate URL
getDatasetCollection <- function () {
    resp <- GET(sessionURL("datasets_list_view"))
    return(resp$body$active) ## we may want to also support archived datasets
}

dataset_collection <- function () session_store$datasets

##' Load a Crunch Dataset
##' @param dataset.name character, the name of a Crunch dataset you have access to. 
##' @param dataset.list, the local session store from which to retrieve the dataset's resource URLs. You shouldn't change this.
##' @return An object of class \code{CrunchDataset}
##' @export 
loadDataset <- function (dataset.name) {
    dataset <- selectDatasetFromCollection(dataset.name, dataset_collection())
    return(as.dataset(GET(dataset$datasetUrl)))
}

selectDatasetFromCollection <- function (dsname, dslist=list()) {
    found <- Filter(function (x) x$datasetName == dsname, dslist)
    if (length(found)==0) {
        stop(paste(dsname, "not found"), call.=FALSE)
    } else if (length(found) > 1) {
        warning("Datasets with duplicate names found. Returning first match.",
            call.=FALSE)
        # stop("Datasets with duplicate names found. Cannot select by name.", call.=FALSE)
    } 
    return(found[[1]])
}
