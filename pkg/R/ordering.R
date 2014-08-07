##' Get and set VariableOrder
##'
##' The \code{ordering} methods allow you to get and set a
##' \code{\link{VariableOrder}} on
##' a \code{\link{CrunchDataset}} or on the \code{\link{VariableCatalog}} that
##' the dataset contains.
##'
##' @param x a VariableCatalog or CrunchDataset
##' @param value a valid VariableOrder object
##' @return \code{ordering} returns a VariableOrder object, while \code{ordering<-} sets the VariableOrder in \code{value} on \code{x}
##' @rdname ordering
##' @export
setMethod("ordering", "CrunchDataset", function (x) ordering(variables(x)))

##' @rdname ordering
##' @export
setMethod("ordering<-", "CrunchDataset", function (x, value) {
    ordering(variables(x)) <- value
    return(x)
})

##' @rdname ordering
##' @export
setMethod("ordering", "VariableCatalog", function (x) x@order)

##' @rdname ordering
##' @export
setMethod("ordering<-", "VariableCatalog", function (x, value) {
    stopifnot(inherits(value, "VariableOrder"))
    PUT(x@views$hierarchical_order, body=toJSON(list(groups=value)))
    x@order <- do.call(VariableOrder,
        GET(x@views$hierarchical_order)$groups)
    return(x)
})