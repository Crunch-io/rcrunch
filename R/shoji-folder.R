setMethod("initialize", "ShojiFolder", function (.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@graph <- lapply(.Object@graph, absoluteURL, .Object@self)
    .Object@index <- .Object@index[unlist(.Object@graph)]
    return(.Object)
})

is.folder <- function (x) inherits(x, "ShojiFolder")

#' @export
#' @rdname describe-catalog
setMethod("types", "ShojiFolder", function (x) getIndexSlot(x, "type"))

#' @rdname catalog-extract
#' @export
setMethod("[[", c("ShojiFolder", "numeric"), function (x, i, ..., drop=FALSE) {
    out <- index(x)[i]
    if (is.null(out[[1]])) {
        return(NULL)
    }
    return(folderExtraction(x, out))
})

#' @rdname catalog-extract
#' @export
setMethod("[[", c("ShojiFolder", "character"), function (x, i, ..., drop=FALSE) {
    path <- parseFolderPath(i)
    create <- isTRUE(list(...)$create)
    while (length(path)) {
        ## Recurse
        segment <- path[1]
        this <- callNextMethod(x, segment)
        if (is.null(this) && create) {
            u <- createFolder(x, segment)
            this <- new(class(x), crGET(u))
        } else if (!is.folder(this) && length(path) > 1) {
            ## Can't recurse deeper if this isn't a folder
            halt(deparse(i), " is an invalid path: ", segment, " is not a folder")
        }
        path <- path[-1]
        x <- this
    }
    return(this)
})

#' @rdname describe
#' @export
setMethod("name<-", "ShojiFolder",
    function (x, value) setEntitySlot(x, "name", value))

createFolder <- function (where, name, index, ...) {
    ## TODO: include index of variables/folders in a single request;
    ## turn index into index + graph in payload
    ## TODO: also for reordering, function that takes a list (index) and returns
    ## list(index=index, graph=names(index))
    crPOST(self(where), body=toJSON(list(
        element="shoji:catalog",
        body=list(
            name=name,
            ...
        )
    )))
}

# setMethod("folderExtraction", "ShojiFolder", function (x, tuple) {
#     ## Default method: return a folder of the same type
#     return(get(class(x))(crGET(names(tuple))))
# })
