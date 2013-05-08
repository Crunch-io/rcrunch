##' Upload a data.frame to Crunch to make a new dataset
##' @param x a data.frame or other rectangular R object
##' @param name character, the name to give the new Crunch dataset. Default is the name of the R object passed in \code{x}
##' @param ... additional arguments passed to \code{ \link{newDatasetFromFile}}
##' @return If successful, an object of class CrunchDataset.
##' @export
newDataset <- function (x, name=substitute(x), ...) {
    
    is.2D <- !is.null(dim(x)) && length(dim(x)) %in% 2
    if (!is.2D) {
        stop("Can only make a Crunch dataset from a two-dimensional data ",
            "structure", call.=FALSE)
    }
    
    # v1: dump a csv, then route through newDatasetFromFile. 
    # then manipulate the remote dataset
    # later, we'll want to serialize some other way that preserves metadata
    
    ## This code is data.frame specific. need to modify for matrix type
    
    ## First, collect original type information
    vartypes <- crunchType(x)
    are.factor <- vapply(x, is.factor, logical(1))
    if (any(are.factor)) {
        factor.levels <- lapply(x[are.factor], levels)
        x[are.factor] <- lapply(x[are.factor], as.numeric)
    }
    
    file <- tempfile(fileext=".csv")
    write.csv(x, file=file, row.names=FALSE)
    crunchdf <- newDatasetFromFile(file, name=as.character(name), ...)
    
    ## Update variable types based on what we know
    crunchdf[] <- mapply(castVariable, x=crunchdf, to=vartypes)
    if (any(are.factor)) {
        
    }
    invisible(crunchdf)
}

##' Upload a file to Crunch to make a new dataset
##' @param file character, the path to a file to upload
##' @param name character, the name to give the new Crunch dataset. Default is the file name
##' @return If successful, an object of class crunchdf.
##' @param ... additional arguments, currently not implemented
##' @export 
newDatasetFromFile <- function (file, name=basename(file), ...) {
    if (!file.exists(file)) {
        stop("File not found", call.=FALSE)
    }
    source <- createSource(file)
    ds <- createDataset(name)
    addSourceToDataset(ds, source)
    updateDatasetList()
    invisible(as.dataset(GET(ds)))
}

##' @importFrom httr upload_file
createSource <- function (file, ...) {
    POST(sessionURL("sources_url"), body=list(uploaded_file=upload_file(file)),
        ...)
}

createDataset <- function (name, ...) {
    POST(sessionURL("datasets_url"), body=toJSON(list(name=name)), ...)
}

addSourceToDataset <- function (dataset_url, source_url, ...) {
    ds <- GET(dataset_url)
    POST(ds$urls$sources_url, body=toJSON(list(source_url=source_url)), ...)
}

