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
setMethod("ordering", "VariableCatalog", function (x) x@order)

##' @rdname ordering
##' @export
setMethod("ordering<-", "VariableCatalog", function (x, value) {
    stopifnot(inherits(value, "VariableOrder"))
    ## Validate.
    new.entities <- entities(value)
    bad.entities <- setdiff(new.entities, urls(x))
    if (length(bad.entities)) {
        halt("Variable URL", ifelse(length(bad.entities) > 1, "s", ""),
            " referenced in Order not present in catalog: ", 
            serialPaste(bad.entities))
    }
    
    crPUT(x@views$hierarchical_order, body=toJSON(value))
    x@order <- VariableOrder(crGET(x@views$hierarchical_order))

    return(x)
})