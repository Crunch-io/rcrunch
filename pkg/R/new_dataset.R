newDataset <- function (x, name=substitute(x), ...) {
    # v1: dump a csv, then route through newDatasetFromFile.
    # later, we'll want to serialize some other way that preserves metadata
    file <- tempfile(fileext=".csv")
    write.csv(x, file=file, row.names=FALSE)
    invisible(newDatasetFromFile(file, name=name, ...))
}

##' Upload a file to Crunch to make a new dataset
##' @param file character, the path to a file to upload
##' @param name character, the name to give the new Crunch dataset. Default is the file name
##' @return If successful, an object of class crunchdf.
##' @export 
newDatasetFromFile <- function (file, name=basename(file), ...) {
    if (!file.exists(file)) {
        stop("File not found", call.=FALSE)
    }
    source <- createSource(file)
    ds <- createDataset(name)
    addSourceToDataset(ds, source)
    updateDatasetList()
    invisible(name)
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

