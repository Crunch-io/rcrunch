setMethod("initialize", "VariableCatalog", function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    h_url <- .Object@orders$hier
    if (!is.null(h_url)) {
        o <- crGET(h_url, query = list(relative = "on"))
        .Object@order <- VariableOrder(o)
        ## Sort catalog based on the order, but be forgiving if the order isn't
        ## 100% trustworthy
        order_urls <- unique(urls(.Object@order))
        index_urls <- names(.Object@index)
        .Object@index <- .Object@index[c(
            intersect(order_urls, index_urls),
            setdiff(index_urls, order_urls)
        )]
    }
    return(.Object)
})

#' @rdname crunch-extract
#' @export
setMethod("[[", c("VariableCatalog", "numeric"), function(x, i, ...) {
    getTuple(x, i, VariableTuple)
})
#' @rdname crunch-extract
#' @export
setMethod(
    "[[<-", c("VariableCatalog", "character", "missing", "VariableTuple"),
    function(x, i, j, value) {
        index(x)[[i]] <- value@body
        return(x)
    }
)
#' @rdname crunch-extract
#' @export
setMethod(
    "[[<-", c("VariableCatalog", "character", "missing", "CrunchVariable"),
    function(x, i, j, value) {
        stopifnot(i == self(value))
        x[[i]] <- tuple(value)
        return(x)
    }
)
#' @rdname crunch-extract
#' @export
setMethod("[", c("VariableCatalog", "VariableOrder"), function(x, i, ...) {
    index(x) <- index(x)[urls(i)]
    return(x)
})
#' @rdname crunch-extract
#' @export
setMethod("[", c("VariableCatalog", "VariableGroup"), function(x, i, ...) {
    index(x) <- index(x)[urls(i)]
    return(x)
})

#' @rdname crunch-extract
#' @export
setMethod(
    "[<-", c("VariableCatalog", "VariableOrder", "missing", "VariableCatalog"),
    function(x, i, j, value) {
        i <- urls(i)
        callNextMethod(x, i, value = value)
    }
)
#' @rdname crunch-extract
#' @export
setMethod(
    "[<-", c("VariableCatalog", "VariableGroup", "missing", "VariableCatalog"),
    function(x, i, j, value) {
        i <- urls(i)
        callNextMethod(x, i, value = value)
    }
)

#' @export
#' @rdname describe-catalog
setMethod("aliases", "VariableCatalog", function(x) getIndexSlot(x, "alias"))
#' @export
#' @rdname describe-catalog
setMethod("aliases<-", "VariableCatalog", function(x, value) {
    setIndexSlot(x, "alias", value)
})
#' @export
#' @rdname describe-catalog
setMethod("notes", "VariableCatalog", function(x) getIndexSlot(x, "notes", ifnot = ""))
#' @export
#' @rdname describe-catalog
setMethod("notes<-", "VariableCatalog", function(x, value) {
    setIndexSlot(x, "notes", value)
})

#' @export
#' @rdname describe-catalog
setMethod(
    "descriptions", "VariableCatalog",
    function(x) getIndexSlot(x, "description", ifnot = "")
)
#' @export
#' @rdname describe-catalog
setMethod("descriptions<-", "VariableCatalog", function(x, value) {
    setIndexSlot(x, "description", value)
})

#' @export
#' @rdname describe-catalog
setMethod("types", "VariableCatalog", function(x) getIndexSlot(x, "type"))

#' @export
#' @rdname describe-catalog
setMethod("ids", "VariableCatalog", function(x) getIndexSlot(x, "id"))

## No setter for types<- or ids<-
