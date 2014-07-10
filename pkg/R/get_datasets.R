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

selectDatasetFromCatalog <- function (dsname, catalog, strict=FALSE) {
    found <- which(names(catalog) %in% dsname)
    if (length(found)==0) {
        stop(paste(dQuote(dsname), "not found"), call.=FALSE)
    } else if (length(found) > 1) {
        if (strict) {
            stop("Datasets with duplicate names found. Cannot select by name.",
                call.=FALSE)
        } else {
            warning(paste("Datasets with duplicate names found.",
                "Returning first match."), call.=FALSE)
        }
    } 
    return(found[1])
}

##' Delete a dataset from the dataset list
##'
##' This function lets you delete a dataset without first loading it. If you
##' have a dataset that somehow is corrupted and won't load, you can delete it
##' this way. 
##'
##' The function also works on CrunchDataset objects, just like
##' \code{\link{delete}}, which may be useful if you have loaded another 
##' package that masks the \code{delete} method.
##' @param x The name (character) of a dataset, its (numeric) position in the
##' return of \code{\link{listDatasets}}, or an object of class
##' \code{CrunchDataset}. x can only be of length 1--this function is not 
##' vectorized (for your protection).
##' @return (Invisibly) the API response from deleting the dataset
##' @export
deleteDataset <- function (x) {
    if (is.dataset(x)) {
        out <- delete(x)
    } else {
        if (!is.numeric(x)) {
            x <- selectDatasetFromCatalog(x, datasetCatalog(), strict=TRUE)
        }
        stopifnot(length(x) == 1)
        
        ds.tuple <- datasetCatalog()[[x]]
        if (interactive()) {
            message(paste("Deleting dataset", dQuote(name(ds.tuple)), 
                "in 5 seconds. Press ctrl-C to abort."))
            Sys.sleep(5)
        }
        out <- DELETE(ds.tuple@entity_url)
    }

    updateDatasetList()
    invisible(out)
}
