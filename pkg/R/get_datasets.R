##' Show the names of all Crunch datasets
##' @param refresh logical: should the function check the Crunch API for new datasets? Default is FALSE. 
##' @return Character vector of dataset names, each of which would be a valid input for \code{\link{loadDataset}}
##' @export 
listDatasets <- function (kind=c("active", "all", "archived"), refresh=FALSE) {
    if (refresh && crunchAPIcanBeReached()) {
        updateDatasetList()
    }
    return(names(subsetDatasetCatalog(match.arg(kind))))
}

subsetDatasetCatalog <- function (kind, catalog=datasetCatalog()) {
    return(switch(kind, 
        active=active(catalog),
        all=catalog,
        archived=archived(catalog)))
}

##' Refresh the local list of Crunch datasets
##' @return Nothing. Called for its side effects of setting local environment variables.
##' @export 
updateDatasetList <- function () {
    session_store$datasets <- do.call(DatasetCatalog,
        GET(sessionURL("datasets_url")))
}

datasetCatalog <- function () session_store$datasets

##' Load a Crunch Dataset
##' @param dataset.name character, the name of a Crunch dataset you have access to. 
##' @param dataset.list, the local session store from which to retrieve the dataset's resource URLs. You shouldn't change this.
##' @param useAlias logical whether variable alias or name should be used as R variable names when the dataset is returned. Default is TRUE, meaning alias. They're more computer friendly.
##' @return An object of class \code{CrunchDataset}
##' @export 
loadDataset <- function (dataset.name, kind=c("active", "all", "archived"), useAlias=default.useAlias()) {
    dss <- subsetDatasetCatalog(match.arg(kind))
    
    if (!is.numeric(dataset.name)) {
        dataset.name <- selectDatasetFromCatalog(dataset.name, dss)
    }
    stopifnot(length(dataset.name) == 1)

    dataset <- entity(dss[[dataset.name]])
    dataset@useAlias <- useAlias
    return(dataset)
}

selectDatasetFromCatalog <- function (dsname, catalog) {
    found <- which(names(catalog) %in% dsname)
    if (length(found)==0) {
        stop(paste(dsname, "not found"), call.=FALSE)
    } else if (length(found) > 1) {
        warning("Datasets with duplicate names found. Returning first match.",
            call.=FALSE)
        # stop("Datasets with duplicate names found. Cannot select by name.", call.=FALSE)
    } 
    return(found[1])
}
