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
    crPOST(sessionURL("sources"),
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
##' @seealso newDataset
##' @keywords internal
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

##' Upload a data.frame to Crunch to make a new dataset
##'
##' This function uses the CSV+JSON import format, which may be faster or more
##' effective for certain dataset sizes and shapes. 
##'
##' @param x a data.frame or other rectangular R object
##' @param name character, the name to give the new Crunch dataset. Default is
##' the name of the R object passed in \code{x}
##' @param useAlias logical whether variable alias or name should be used as R
##' variable names when the dataset is returned. Default is TRUE, meaning alias.
##' @param ... additional arguments passed to \code{ \link{createDataset}}
##' @return If successful, an object of class CrunchDataset.
##' @seealso \code{\link{newDataset}}
##' @export
newDataset2 <- function (x, name=substitute(x),
                                useAlias=default.useAlias(), ...) {
    
    is.2D <- !is.null(dim(x)) && length(dim(x)) %in% 2
    if (!is.2D) {
        halt("Can only make a Crunch dataset from a two-dimensional data ",
            "structure")
    }
    
    ## Get all the things
    message("Processing the data")
    vars <- lapply(names(x), 
        function (i) toVariable(x[[i]], name=i, alias=i))
    names(vars) <- names(x)
    
    ## Extract the data
    cols <- lapply(vars, function (v) v[["values"]])
    filename <- tempfile()
    gf <- gzfile(filename, "w")
    write.csv(cols, file=gf, na="", row.names=FALSE)
    close(gf)
    
    ## Drop the columns from the metadata and compose the payload
    vars <- lapply(vars, function (v) {
        v[["values"]] <- NULL
        return(v)
    })
    meta <- shojifyMetadata(vars, name=name, ...)
    
    ## Send to Crunch
    ds <- createWithMetadataAndFile(meta, filename)
    ds@useAlias <- useAlias
    invisible(ds)
}

##' Make a dataset with metadata and a CSV
##'
##' This function just takes what you give it and POSTs to Crunch. No
##' validation, no automatic wrapping in the Shoji envelope, etc.
##'
##' @param metadata a list representation of the dataset's metadata, which
##' will be JSON-serialized and POSTed.
##' @param file a path to a CSV file, optionally zipped, that corresponds to
##' the above metadata.
##' @return On success, a new dataset.
createWithMetadataAndFile <- function (metadata, file) {
    message("Uploading metadata")
    dataset_url <- crPOST(sessionURL("datasets"), body=toJSON(metadata))
    updateDatasetList()
    ds <- entity(datasetCatalog()[[dataset_url]])
    
    message("Uploading data")
    batches_url <- ds@catalogs$batches
    crPOST(batches_url,
        body=list(file=upload_file(file)))
    message("Done!")
    return(refresh(ds))
}

##' Wrap variable metadata inside a dataset entity
##'
##' @param metadata list of variable metadata
##' @param ... dataset entity metadata. "name" is required.
##' @param return list suitiable for JSONing and POSTing to create a dataset
shojifyMetadata <- function (metadata, ...) {
    return(list(element="shoji:entity", 
                 body=list(..., 
                           table=list(element="crunch:table",
                                      metadata=metadata))))
}

