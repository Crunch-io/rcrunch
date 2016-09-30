#' @rdname tuple-methods
#' @export
setMethod("$", "ShojiEntity", function (x, name) x@body[[name]])
#' @rdname tuple-methods
#' @export
setMethod("$<-", "ShojiEntity", function (x, name, value) {
    x@body[[name]] <- value
    return(x)
})
#' @rdname tuple-methods
#' @export
setMethod("[[", "ShojiEntity", function (x, i) x@body[[i]])
#' @rdname tuple-methods
#' @export
setMethod("[[<-", "ShojiEntity", function (x, i, value) {
    x@body[[i]] <- value
    return(x)
})
