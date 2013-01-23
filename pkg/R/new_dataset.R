newDataset <- function (file, name=basename(file)) {
    if (!file.exists(file)) {
        stop("File not found", call.=FALSE)
    }
    source <- createSource(file)
    ds <- createDataset(name)
    addSourceToDataset(ds, source)
    updateDatasetList()
}

##' @importFrom httr upload_file
##' @export 
createSource <- function (file) {
    out <- POST(session_store$urls$sources_url, body=list(uploaded_file=upload_file(file)))
    return(out)
}

##' @export 
createDataset <- function (name) {
    out <- POST(session_store$urls$datasets_url, body=list(name=name))
    return(out)
}

addSourceToDataset <- function (dataset_url, source_url) {
    ds <- GET(dataset_url)
    crunch.API("POST", ds$urls$actions_url, )  ###
}
