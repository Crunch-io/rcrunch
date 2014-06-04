##' Upload a data.frame to Crunch to make a new dataset
##' @param x a data.frame or other rectangular R object
##' @param name character, the name to give the new Crunch dataset. Default is the name of the R object passed in \code{x}
##' @param ... additional arguments passed to \code{ \link{newDatasetFromFile}}
##' @return If successful, an object of class CrunchDataset.
##' @export
newDataset <- function (x, name=substitute(x),
                                useAlias=default.useAlias(), ...) {

    is.2D <- !is.null(dim(x)) && length(dim(x)) %in% 2
    if (!is.2D) {
        stop("Can only make a Crunch dataset from a two-dimensional data ",
            "structure", call.=FALSE)
    }
    
    crunchdf <- createDataset(name=name, useAlias=useAlias, ...)
    crunchdf <- addVariables(crunchdf, x)
    invisible(crunchdf)
}

newDatasetViaFile <- function (x, name=substitute(x), ...) {
    
    is.2D <- !is.null(dim(x)) && length(dim(x)) %in% 2
    if (!is.2D) {
        stop("Can only make a Crunch dataset from a two-dimensional data ",
            "structure", call.=FALSE)
    }
    
    # v1: dump a csv, then route through newDatasetFromFile. 
    # then manipulate the remote dataset
    # later, we'll want to serialize some other way that preserves metadata
    
    ## This code is data.frame specific. need to modify for matrix type
    
    file <- tempfile(fileext=".csv")
    write.csv(preUpload(x), file=file, row.names=FALSE)
    crunchdf <- newDatasetFromFile(file, name=as.character(name), ...)
    
    ## Update variable types based on what we know
    # crunchdf[] <- 
    sapply(names(crunchdf), function (i) {
        postUpload(x[[i]], crunchdf[[i]])
    }, simplify=FALSE)
    
    ## You can write methods for preUpload and postUpload for any variable type
    
    invisible(refresh(crunchdf))
}

##' Upload a file to Crunch to make a new dataset
##' @param file character, the path to a file to upload
##' @param name character, the name to give the new Crunch dataset. Default is the file name
##' @return If successful, an object of class crunchdf.
##' @param useAlias logical whether variable alias or name should be used as R variable names when the dataset is returned. Default is TRUE, meaning alias. They're more computer friendly.
##' @param ... additional arguments, currently not implemented
##' @export 
newDatasetFromFile <- function (file, name=basename(file),
                                useAlias=default.useAlias(), ...) {
    if (!file.exists(file)) {
        stop("File not found", call.=FALSE)
    }
    ds <- createDataset(name, useAlias=useAlias)
    ds <- addSourceToDataset(ds, createSource(file))
    invisible(ds)
}

##' @importFrom httr upload_file
createSource <- function (file, ...) {
    POST(sessionURL("sources_url"), body=list(uploaded_file=upload_file(file)),
        ...)
}

createDataset <- function (name, useAlias=default.useAlias(), ...) {
    dataset_url <- POST(sessionURL("datasets_url"), body=toJSON(list(name=name, ...)))
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
            workflow=I(list())
        )
    )
    batch_url <- POST(batches_url, body=toJSON(body), ...)
    status <- pollBatchStatus(batch_url, ShojiCatalog(GET(batches_url)),
        until="ready")
    if (status != "ready") {
        stop("Error importing file", call.=FALSE)
    }
    PATCH(batch_url, body=toJSON(list(status="importing")))
    pollBatchStatus(batch_url, ShojiCatalog(GET(batches_url)))
    invisible(refresh(dataset))
}

.delete_all_my_datasets <- function () {
    lapply(names(datasetCatalog()@index), DELETE)
}
