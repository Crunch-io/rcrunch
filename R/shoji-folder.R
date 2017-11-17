setMethod("initialize", "ShojiFolder", function (.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@graph <- lapply(.Object@graph, absoluteURL, .Object@self)
    .Object@index <- .Object@index[unlist(.Object@graph)]
    return(.Object)
})

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
    i <- parseFolderPath(i)
    while (length(i) > 1) {
        ## Recurse
        x <- x[[i[1]]]
        i <- i[-1]
    }
    return(callNextMethod(x, i))
})

#' @rdname describe
#' @export
setMethod("name<-", "ShojiFolder",
    function (x, value) setEntitySlot(x, "name", value))

# setMethod("folderExtraction", "ShojiFolder", function (x, tuple) {
#     ## Default method: return a folder of the same type
#     return(get(class(x))(crGET(names(tuple))))
# })
