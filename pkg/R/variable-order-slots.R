##' Manipulate VariableGroup and VariableOrder
##'
##' @param x a VariableGroup or VariableOrder
##' @param value For name, a character (length-1 vector); for names, a character
##' vector of equal length to the number of VariableGroups beind modified; for
##' entities, either a character vector of variable URLs or a list containing a
##' combination of variable URLs and VariableGroups. Note that group names must
##' be unique, should be greater than 0 characters long, and "ungrouped" is a
##' reserved group name.
##' @param unlist logical: should variable URLs inside of groups be flattened or
##' preserved in their nested lists? Default is \code{TRUE}, meaning flattened.
##' @return Variable references, VariableGroups, or group names, as appropriate.
##' @rdname VariableOrder-slots
##' @seealso VariableOrder
##' @seealso grouped
##' @aliases entities
##' @export
setMethod("entities", "VariableGroup", function (x, unlist=TRUE) {
    out <- x@entities
    if (is.list(out)) {
        nested.groups <- vapply(out, 
            function (a) inherits(a, "VariableGroup"), logical(1))
        out[nested.groups] <- lapply(out[nested.groups], 
            function (a) entities(a))
        if (unlist) out <- unique(unlist(out))
    }
    return(out)
})
##' @rdname VariableOrder-slots
##' @export
setMethod("entities", "VariableOrder", function (x, unlist=TRUE) {
    ## To get a flattened view
    es <- lapply(x, function (a) entities(a))
    if (unlist) {
        es <- unique(unlist(es))
    }
    return(es)
})
##' @rdname VariableOrder-slots
##' @export
setMethod("entities<-", "VariableGroup", function (x, value) {
    x@entities <- .initEntities(value)
    return(x)
})

##' @rdname VariableOrder-slots
##' @export
setMethod("name", "VariableGroup", function (x) x@group)
##' @rdname VariableOrder-slots
##' @export
setMethod("name<-", "VariableGroup", function (x, value) {
    x@group <- value ## Should check that we're not renaming "ungrouped"
    return(x)
})

##' @rdname VariableOrder-slots
##' @export
setMethod("names", "VariableOrder", 
    function (x) vapply(x, function (a) name(a), character(1)))
##' @rdname VariableOrder-slots
##' @export
setMethod("names<-", "VariableOrder", 
    function (x, value) {
        x@value$groups <- mapply(
            function (y, v) {
                y@group <- v
                return(y)
            }, y=x@value$groups, v=value, SIMPLIFY=FALSE, USE.NAMES=FALSE)
        return(x)
    })
