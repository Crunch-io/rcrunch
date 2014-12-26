##' @rdname toVariable
##' @export
setMethod("toVariable", "character", function (x, ...) {
    return(list(values=x, type="text", ...))
})
##' @rdname toVariable
##' @export
setMethod("toVariable", "numeric", function (x, ...) {
    return(list(values=x, type="numeric", ...))
})
##' @rdname toVariable
##' @export
setMethod("toVariable", "factor", function (x, ...) {
    nlevels <- length(levels(x))
    max.categories <- getOption("crunch.max.categories")
    if (!is.null(max.categories) && nlevels > max.categories) {
        return(toVariable(as.character(x), ...))
    } 
    out <- list(values=as.integer(x), type="categorical",
        categories=categoriesFromLevels(levels(x)), ...)
    return(NAToCategory(out))
})
##' @rdname toVariable
##' @export
setMethod("toVariable", "Date", function (x, ...) {
    return(list(values=as.character(x), type="datetime", resolution="D", ...))
})
# setMethod("toVariable", "POSIXt", function (x) {
#     return(list(values=as.character(x), type="datetime", resolution="s", ...))
# })

##' @rdname toVariable
##' @export
setMethod("toVariable", "logical", function (x, ...) {
    ## Make it categorical
    out <- list(values=2L-as.integer(x), type="categorical", 
        categories=categoriesFromLevels(c("True", "False")),
        ...)
    return(NAToCategory(out))
})

categoriesFromLevels <- function (x) {
    return(lapply(seq_along(x), function (i) {
        list(id=i, name=x[i], numeric_value=i, missing=FALSE)
    }))
}

NAToCategory <- function (var.metadata) {
    if (any(is.na(var.metadata$values))) {
        var.metadata$values[is.na(var.metadata$values)] <- -1L
        var.metadata$categories[[length(var.metadata$categories)+1]] <- .no.data
    }
    return(var.metadata)
}