##' Export a dataset to a file
##'
##' @param dataset CrunchDataset
##' @param file character local filename to write to
##' @param format character export format: currently supported values are "csv"
##' and "spss".
##' @param ... additional arguments, currently ignored
##' @return Invisibly, \code{file}.
##' @export
##' @importFrom utils download.file
exportDataset <- function (dataset, file, format=c("csv", "spss"), ...) {
    exporters <- crGET(shojiURL(dataset, "views", "export"))
    format <- match.arg(format, choices=names(exporters))
    export_url <- exporters[[format]]
    body <- list(filter=zcl(activeFilter(dataset)))
    ## Check to see if we have a subset of variables in `dataset`
    allvars <- allVariables(dataset)
    if (length(allvars) != length(ShojiCatalog(crGET(self(allvars))))) {
        body$where <- list(`function`="identify", args=list(list(id=urls(allvars))))
    }
    result <- crPOST(export_url, body=toJSON(body))
    download.file(result$url, file, quiet=TRUE) ## Note outside of auth. Ok because file is in s3 with token
    invisible(file)
}

##' @rdname exportDataset
##' @export
setMethod("write.csv", "CrunchDataset", function (...) exportDataset(..., format="csv"))
