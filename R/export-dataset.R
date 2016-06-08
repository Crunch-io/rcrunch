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
    result <- crGET(export_url)
    download.file(result$url, file, quiet=TRUE) ## Note outside of auth. Ok because file is in s3 with token
    invisible(file)
}

##' @rdname exportDataset
##' @export
setMethod("write.csv", "CrunchDataset", function (...) exportDataset(..., format="csv"))
