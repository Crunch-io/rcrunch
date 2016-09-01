#' Manipulate VariableGroup and VariableOrder
#'
#' @param x a VariableGroup or VariableOrder
#' @param value (1) For name, a character (length-1 vector); for names, a character
#' vector of equal length to the number of VariableGroups beind modified; for
#' entities, either a character vector of variable URLs or a list containing a
#' combination of variable URLs and VariableGroups. Note that group names must
#' be unique, should be greater than 0 characters long, and "ungrouped" is a
#' reserved group name. (2) For duplicates, logical for whether duplicate
#' variable entries should be allowed in the VariableOrder.
#' @param simplify logical: should variable URLs inside of groups be flattened
#' or preserved in their nested lists? Default is \code{FALSE}.
#' @return \code{entities} returns Variable references and VariableGroups; \code{names} returns group names; \code{duplicates} returns logical for whether duplicate variable entries should be allowed
#' @name ShojiOrder-slots
#' @seealso \code{\link{VariableOrder}}
#' @seealso \code{\link{grouped}}
#' @aliases entities entities<- duplicates duplicates<-
NULL

#' @rdname ShojiOrder-slots
#' @export
setMethod("entities", "OrderGroup", function (x, simplify=FALSE) {
    entities(x@entities, simplify=simplify)
})
#' @rdname ShojiOrder-slots
#' @export
setMethod("entities", "ShojiOrder", function (x, simplify=FALSE) {
    ## To get a flattened view
    entities(x@graph, simplify=simplify)
})
#' @rdname ShojiOrder-slots
#' @export
setMethod("entities", "list", function (x, simplify=FALSE) {
    if (simplify) {
        nested.groups <- vapply(x, inherits, logical(1), what="OrderGroup")
        x[nested.groups] <- lapply(x[nested.groups],
            function (a) entities(a, simplify=TRUE))
        x <- unique(unlist(x))
    }
    return(x)
})

#' @rdname urls
#' @export
setMethod("urls", "ShojiOrder", function (x) entities(x@graph, simplify=TRUE))
#' @rdname urls
#' @export
setMethod("urls", "OrderGroup", function (x) entities(x, simplify=TRUE))
#' @rdname urls
#' @export
setMethod("urls", "CrunchVariable", function (x) self(x))
#' @rdname urls
#' @export
setMethod("urls", "CrunchDataset", function (x) urls(variables(x)))

#' @rdname ShojiOrder-slots
#' @export
setMethod("entities<-", "OrderGroup", function (x, value) {
    x@entities <- entitiesInitializer(x)(value)
    return(x)
})
#' @rdname ShojiOrder-slots
#' @export
setMethod("entities<-", "ShojiOrder", function (x, value) {
    x@graph <- entitiesInitializer(x)(value)
    return(x)
})

#' @rdname ShojiOrder-slots
#' @export
setMethod("name", "OrderGroup", function (x) x@group)
#' @rdname ShojiOrder-slots
#' @export
setMethod("name<-", "OrderGroup", function (x, value) {
    x@group <- validateNewName(value)
    return(x)
})

.ordernames <- function (x) {
    vapply(x, function (a) {
        ifelse(inherits(a, "OrderGroup"), name(a), NA_character_)
    }, character(1))
}
#' @rdname ShojiOrder-slots
#' @export
setMethod("names", "ShojiOrder", .ordernames)


#' @rdname ShojiOrder-slots
#' @export
setMethod("names", "OrderGroup", .ordernames)

#' @rdname ShojiOrder-slots
#' @export
setMethod("names<-", "ShojiOrder",
    function (x, value) {
        x@graph <- mapply(
            function (y, v) {
                if (!is.na(v)) y@group <- v
                return(y)
            }, y=x@graph, v=value, SIMPLIFY=FALSE, USE.NAMES=FALSE)
        return(x)
    })

#' @rdname ShojiOrder-slots
#' @export
setMethod("duplicates", "ShojiOrder", function (x) x@duplicates)
#' @rdname ShojiOrder-slots
#' @export
setMethod("duplicates", "OrderGroup", function (x) x@duplicates)
#' @rdname ShojiOrder-slots
#' @export
setMethod("duplicates", "VariableCatalog", function (x) duplicates(x@order))
#' @rdname ShojiOrder-slots
#' @export
setMethod("duplicates<-", c("ShojiOrder", "logical"), function (x, value) {
    value <- isTRUE(value) ## To purge NA_logical_
    x@duplicates <- value
    grps <- vapply(x@graph, inherits, logical(1), what="OrderGroup")
    x@graph[grps] <- lapply(x@graph[grps], `duplicates<-`, value=value)
    return(x)
})
#' @rdname ShojiOrder-slots
#' @export
setMethod("duplicates<-", c("OrderGroup", "logical"), function (x, value) {
    value <- isTRUE(value) ## To purge NA_logical_
    x@duplicates <- value
    grps <- vapply(x@entities, inherits, logical(1), what="OrderGroup")
    x@entities[grps] <- lapply(x@entities[grps], `duplicates<-`, value=value)
    return(x)
})
#' @rdname ShojiOrder-slots
#' @export
setMethod("duplicates<-", c("VariableCatalog", "logical"), function (x, value) {
     duplicates(x@order) <- isTRUE(value) ## To purge NA_logical_
     return(x)
})
