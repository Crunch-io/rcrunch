##' Upload a data.frame to Crunch to make a new dataset
##'
##' @param x a data.frame or other rectangular R object
##' @param name character, the name to give the new Crunch dataset. Default is
##' the name of the R object passed in \code{x}
##' @param useAlias logical whether variable alias or name should be used as R
##' variable names when the dataset is returned. Default is TRUE, meaning alias.
##' @param ... additional arguments passed to \code{ \link{createDataset}}
##' @return If successful, an object of class CrunchDataset.
##' @export
newDataset <- function (x, name=substitute(x),
                                useAlias=default.useAlias(), ...) {

    is.2D <- !is.null(dim(x)) && length(dim(x)) %in% 2
    if (!is.2D) {
        halt("Can only make a Crunch dataset from a two-dimensional data ",
            "structure")
    }
    
    ds <- createDataset(name=name, useAlias=useAlias, ...)
    ds <- addVariables(ds, x)
    invisible(ds)
}

##' Upload a file to Crunch to make a new dataset
##'
##' @param file character, the path to a file to upload
##' @param name character, the name to give the new Crunch dataset. Default is
##' the file name
##' @return If successful, an object of class crunchdf.
##' @param useAlias logical whether variable alias or name should be used as R
##' variable names when the dataset is returned. Default is TRUE, meaning alias.
##' They're more computer friendly.
##' @param ... additional arguments passed to \code{ \link{createDataset}}
##' @export 
newDatasetFromFile <- function (file, name=basename(file),
                                useAlias=default.useAlias(), ...) {
    if (!file.exists(file)) {
        halt("File not found")
    }
    ds <- createDataset(name, useAlias=useAlias, ...)
    ds <- addSourceToDataset(ds, createSource(file))
    invisible(ds)
}

##' @importFrom httr upload_file
createSource <- function (file, ...) {
    source_url <- rootURL("sources", session_store$user) ## BAD!
    crPOST(source_url, #sessionURL("sources"),
        body=list(uploaded_file=upload_file(file)), ...)
}

##' Create an empty dataset
##'
##' Use only if you're writing a function to create a Crunch dataset from a
##' custom data structure. If you have a data.frame, just call
##' \code{\link{newDataset}} on it.
##'
##' @param name character, the name to give the new Crunch dataset. This is
##' required.
##' @param useAlias logical whether variable alias or name should be used as R
##' variable names when the dataset is returned. Default is TRUE, meaning alias.
##' This argument is only relevant for the dataset object that is returned;
##' it has no effect on the contents of the object created on the server.
##' @param ... additional arguments for the POST to create the dataset, such as
##' "description". 
##' @return An object of class CrunchDataset.
##' @export
createDataset <- function (name, useAlias=default.useAlias(), ...) {
    dataset_url <- crPOST(sessionURL("datasets"),
        body=toJSON(list(name=name, ...)))
    updateDatasetList()
    ds <- entity(datasetCatalog()[[dataset_url]])
    ds@useAlias <- useAlias
    invisible(ds)
}

addSourceToDataset <- function (dataset, source_url, ...) {
    batches_url <- dataset@catalogs$batches
    body <- list(
        element="shoji:entity",
        body=list(
            source=source_url,
            workflow=I(list()),
            async=TRUE
        )
    )
    batch_url <- crPOST(batches_url, body=toJSON(body), ...)
    status <- pollBatchStatus(batch_url, ShojiCatalog(crGET(batches_url)),
        until="ready")
    if (status != "ready") {
        halt("Error importing file")
    }
    crPATCH(batch_url, body=toJSON(list(status="importing")))
    pollBatchStatus(batch_url, ShojiCatalog(crGET(batches_url)))
    invisible(refresh(dataset))
}

.delete_all_my_datasets <- function () {
    lapply(urls(datasetCatalog()), crDELETE)
}
