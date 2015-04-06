##' Get and set VariableOrder
##'
##' The \code{ordering} methods allow you to get and set a
##' \code{\link{VariableOrder}} on
##' a \code{\link{CrunchDataset}} or on the \code{\link{VariableCatalog}} that
##' the dataset contains.
##'
##' @param x a VariableCatalog or CrunchDataset
##' @param value a valid VariableOrder object
##' @return \code{ordering} returns a VariableOrder object, while
##' \code{ordering<-} sets the VariableOrder in \code{value} on \code{x}
##' @name ordering
##' @aliases ordering ordering<-
NULL

##' @rdname ordering
##' @export
setMethod("ordering", "CrunchDataset", function (x) ordering(variables(x)))

##' @rdname ordering
##' @export
setMethod("ordering<-", "CrunchDataset", function (x, value) {
    ordering(allVariables(x)) <- value
    return(x)
})

##' @rdname ordering
##' @export
setMethod("ordering", "VariableCatalog", function (x) {
    out <- x@order
    out@vars <- index(x)
    return(out)
})

##' @rdname ordering
##' @export
setMethod("ordering<-", "VariableCatalog", function (x, value) {
    stopifnot(inherits(value, "VariableOrder"))
    ## Validate.
    bad.entities <- setdiff(urls(value), urls(x))
    if (length(bad.entities)) {
        halt("Variable URL", ifelse(length(bad.entities) > 1, "s", ""),
            " referenced in Order not present in catalog: ", 
            serialPaste(bad.entities))
    }
    
    crPUT(x@orders$hier, body=toJSON(value))
    x@order <- VariableOrder(crGET(x@orders$hier))

    return(x)
})