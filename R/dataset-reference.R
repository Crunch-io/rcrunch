#' Get a Crunch object's dataset URL
#'
#' @param x a Crunch object
#' @return The URL of the dataset which contains that object
#' @keywords internal
#' @rdname dataset-reference
#' @export
setGeneric("datasetReference", function(x) standardGeneric("datasetReference"))

#' @rdname dataset-reference
setMethod("datasetReference", "CrunchDataset", function(x) self(x))

#' @rdname dataset-reference
setMethod("datasetReference", "CrunchVariable", function(x) {
    # x@urls$dataset_url
    rootURL(x, "dataset") %||% datasetReference(self(x))
})

#' @rdname dataset-reference
setMethod("datasetReference", "CrunchExpr", function(x) x@dataset_url)

#' @rdname dataset-reference
setMethod("datasetReference", "ShojiObject", function(x) datasetReference(self(x)))

#' @rdname dataset-reference
setMethod("datasetReference", "character", function(x) {
    if (length(x) != 1) {
        return(NULL)
    }
    # check if the URL has /datasets/.*/, or for web app URLs, /dataset/.*
    # regexp is a little liberal, could be [0-9a-f]+, but relaxed for tests
    id <- sub(".*/datasets?/([0-9a-z_]+)/?.*$", "\\1", x)
    if (identical(id, x)) {
        # Not a URL with a dataset id
        return(NULL)
    }
    path <- paste0("datasets/", id, "/")
    return(absoluteURL(path, getOption("crunch.api")))
})

#' @rdname dataset-reference
setMethod("datasetReference", "ANY", function(x) NULL)

setGeneric("APIToWebURL", function(x) standardGeneric("APIToWebURL"))

setMethod("APIToWebURL", "CrunchVariable", function(x) {
    ds_url <- gsub("/api/datasets", "/dataset", datasetReference(x))
    return(paste0(ds_url, "browse?variableId=", id(x)))
})

setMethod("APIToWebURL", "CrunchDataset", function(x) {
    return(paste0(absoluteURL("/", getOption("crunch.api")), "dataset/", id(x)))
})

setMethod("APIToWebURL", "ANY", function(x) {
    halt("Web URL is not available for objects of class ", class(x))
})
