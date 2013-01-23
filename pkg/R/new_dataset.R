##' @export 
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
createSource <- function (file) {
    POST(sessionURL("sources_url"), body=list(uploaded_file=upload_file(file)))
}

##' @export 
createDataset <- function (name) {
    POST(sessionURL("datasets_url"), body=list(name=name))
}

addSourceToDataset <- function (dataset_url, source_url) {
    ds <- GET(dataset_url)
    POST(ds$urls$actions_url, )  ###
}
