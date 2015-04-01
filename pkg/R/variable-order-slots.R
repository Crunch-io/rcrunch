##' Manipulate VariableGroup and VariableOrder
##'
##' @param x a VariableGroup or VariableOrder
##' @param value For name, a character (length-1 vector); for names, a character
##' vector of equal length to the number of VariableGroups beind modified; for
##' entities, either a character vector of variable URLs or a list containing a
##' combination of variable URLs and VariableGroups. Note that group names must
##' be unique, should be greater than 0 characters long, and "ungrouped" is a
##' reserved group name.
##' @param simplify logical: should variable URLs inside of groups be flattened 
##' or preserved in their nested lists? Default is \code{FALSE}.
##' @return Variable references, VariableGroups, or group names, as appropriate.
##' @name VariableOrder-slots
##' @seealso VariableOrder
##' @seealso grouped
##' @aliases entities entities<-
##' @export
setMethod("entities", "VariableGroup", function (x, simplify=FALSE) {
    entities(x@entities, simplify=simplify)
})
##' @rdname VariableOrder-slots
##' @export
setMethod("entities", "VariableOrder", function (x, simplify=FALSE) {
    ## To get a flattened view
    entities(x@graph, simplify=simplify)
})
##' @rdname VariableOrder-slots
##' @export
setMethod("entities", "list", function (x, simplify=FALSE) {
    if (simplify) {
        nested.groups <- vapply(x, 
            function (a) inherits(a, "VariableGroup"), logical(1))
        x[nested.groups] <- lapply(x[nested.groups], 
            function (a) entities(a, simplify=TRUE))
        x <- unique(unlist(x))
    }
    return(x)
})

##' @rdname urls
##' @export
setMethod("urls", "VariableOrder", function (x) entities(x@graph, simplify=TRUE))
##' @rdname urls
##' @export
setMethod("urls", "VariableGroup", function (x) entities(x, simplify=TRUE))

##' @rdname VariableOrder-slots
##' @export
setMethod("entities<-", "VariableGroup", function (x, value) {
    x@entities <- .initEntities(value)
    return(x)
})
##' @rdname VariableOrder-slots
##' @export
setMethod("entities<-", "VariableOrder", function (x, value) {
    x@graph <- .initEntities(value)
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
    function (x) vapply(x, function (a) {
        ifelse(inherits(a, "VariableGroup"), name(a), NA_character_)
    }, character(1)))
##' @rdname VariableOrder-slots
##' @export
setMethod("names<-", "VariableOrder", 
    function (x, value) {
        x@graph <- mapply(
            function (y, v) {
                if (!is.na(v)) y@group <- v
                return(y)
            }, y=x@graph, v=value, SIMPLIFY=FALSE, USE.NAMES=FALSE)
        return(x)
    })
