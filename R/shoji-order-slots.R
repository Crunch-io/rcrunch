#' Manipulate VariableGroup and VariableOrder
#'
#' @param x a VariableGroup or VariableOrder
#' @param value
#' 1. For name, a character (length-1 vector); for names, a character
#' vector of equal length to the number of VariableGroups being modified; for
#' entities, either a character vector of variable URLs or a list containing a
#' combination of variable URLs and VariableGroups. Note that group names must
#' be unique, should be greater than 0 characters long, and "ungrouped" is a
#' reserved group name.
#' 1. For duplicates, logical for whether duplicate
#' variable entries should be allowed in the VariableOrder.
#' @param simplify logical: should variable URLs inside of groups be flattened
#' or preserved in their nested lists? Default is`FALSE`.
#' @return
#' * `entities` returns Variable references and VariableGroups;
#' * `names` returns group names;
#' * `duplicates` returns logical for whether duplicate variable entries should be allowed
#' @name ShojiOrder-slots
#' @seealso [`VariableOrder`] [`grouped`]
#' @aliases entities entities<- duplicates duplicates<-
NULL

#' @rdname ShojiOrder-slots
#' @export
setMethod("entities", "OrderGroup", function(x, simplify = FALSE) {
    entities(x@entities, simplify = simplify)
})
#' @rdname ShojiOrder-slots
#' @export
setMethod("entities", "ShojiOrder", function(x, simplify = FALSE) {
    ## To get a flattened view
    entities(x@graph, simplify = simplify)
})
#' @rdname ShojiOrder-slots
#' @export
setMethod("entities", "list", function(x, simplify = FALSE) {
    if (simplify) {
        nested.groups <- vapply(x, inherits, logical(1), what = "OrderGroup")
        x[nested.groups] <- lapply(
            x[nested.groups],
            function(a) entities(a, simplify = TRUE)
        )
        x <- unique(unlist(x)) %||% c() ## unique(unlist(list())) returns NULL
    }
    return(x)
})

#' @rdname urls
#' @export
setMethod("urls", "ShojiOrder", function(x) entities(x@graph, simplify = TRUE))
#' @rdname urls
#' @export
setMethod("urls", "OrderGroup", function(x) entities(x, simplify = TRUE))
#' @rdname urls
#' @export
setMethod("urls", "CrunchVariable", function(x) self(x))
#' @rdname urls
#' @export
setMethod("urls", "CrunchDataset", function(x) urls(variables(x)))

#' @rdname ShojiOrder-slots
#' @export
setMethod("entities<-", "OrderGroup", function(x, value) {
    x@entities <- entitiesInitializer(x)(value)
    return(x)
})
#' @rdname ShojiOrder-slots
#' @export
setMethod("entities<-", "ShojiOrder", function(x, value) {
    x@graph <- entitiesInitializer(x)(value)
    return(x)
})

#' @rdname ShojiOrder-slots
#' @export
setMethod("name", "OrderGroup", function(x) x@group)
#' @rdname ShojiOrder-slots
#' @export
setMethod("name<-", "OrderGroup", function(x, value) {
    x@group <- validateNewName(value)
    return(x)
})

.ordernames <- function(x) {
    vapply(x, function(a) {
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
setMethod(
    "names<-", "ShojiOrder",
    function(x, value) {
        x@graph <- mapply(
            function(y, v) {
                if (!is.na(v)) y@group <- v
                return(y)
            },
            y = x@graph, v = value, SIMPLIFY = FALSE, USE.NAMES = FALSE
        )
        return(x)
    }
)

#' @rdname ShojiOrder-slots
#' @export
duplicates <- function(x) {
    .Deprecated(msg="'duplicates' is deprecated. Entities can exist in one and only one folder, so this function does nothing.")
    return(FALSE)
}
#' @rdname ShojiOrder-slots
#' @export
"duplicates<-" <- function(x, value) {
    .Deprecated(msg="'duplicates<-' is deprecated. Entities can exist in one and only one folder, so this function does nothing")
    return(x)
}
