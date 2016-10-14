#' Export a dataset to a file
#'
#' @param dataset CrunchDataset
#' @param file character local filename to write to
#' @param format character export format: currently supported values are "csv"
#' and "spss".
#' @param categorical character: export categorical values to CSV as category
#' "name" (default) or "id". Ignored by the SPSS exporter.
#' @param ... additional arguments, currently ignored
#' @return Invisibly, \code{file}.
#' @export
#' @importFrom utils download.file
exportDataset <- function (dataset, file, format=c("csv", "spss"),
                           categorical=c("name", "id"), ...) {

    exporters <- crGET(shojiURL(dataset, "views", "export"))
    format <- match.arg(format, choices=names(exporters))
    export_url <- exporters[[format]]

    body <- list(filter=zcl(activeFilter(dataset)))
    ## Add this after so that if it is NULL, the "where" key isn't present
    body$where <- variablesFilter(dataset)

    ## Assemble options
    opts <- list()
    if (format == "csv") {
        opts$use_category_ids <- match.arg(categorical) == "id"
    }
    if (length(opts)) {
        body$options <- opts
    }

    result <- crPOST(export_url, body=toJSON(body))
    download.file(result, file, quiet=TRUE, method="curl") ## Note outside of auth. Ok because file is in s3 with token
    invisible(file)
}

variablesFilter <- function (dataset) {
    ## Check to see if we have a subset of variables in `dataset`.
    ## If so, return a Crunch expression to filter them
    allvars <- allVariables(dataset)
    if (length(allvars) != length(ShojiCatalog(crGET(self(allvars))))) {
        v <- structure(lapply(urls(allvars), function (x) list(variable=x)),
            .Names=ids(allvars))
        return(list(`function`="select", args=list(list(map=v))))
    }
    ## Else, return NULL
    return(NULL)
}

#' @rdname exportDataset
#' @export
setMethod("write.csv", "CrunchDataset", function (...) exportDataset(..., format="csv"))
