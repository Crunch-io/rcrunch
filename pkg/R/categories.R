validCategories <- function (object) {
    are.cats <- vapply(object, is.category, logical(1))
    if (!all(are.cats)) {
        badcount <- sum(!are.cats)
        return(paste0("Invalid categories: ", badcount, 
            ifelse(badcount>1, 
                " elements are not Crunch category objects.", 
                " element is not a Crunch category object.")))
    }
    if (any(duplicated(names(object)))) {
        return("Invalid category names: must be unique")
    }
    if (any(duplicated(ids(object)))) {
        return("Invalid category ids: must be unique")
    }
    return(TRUE)
}
setValidity("Categories", validCategories)

init.Categories <- function (.Object, ...) {
    .Object@.Data <- lapply(..1, Category)
    validObject(.Object)
    return(.Object)
}
setMethod("initialize", "Categories", init.Categories)

is.categories <- function (x) inherits(x, "Categories")

##' @rdname Categories
##' @export
setMethod("[", c("Categories", "ANY"), function (x, i, ...) {
    x@.Data <- x@.Data[i]
    return(x)
})
##' @rdname Categories
##' @export
setMethod("[<-", c("Categories", "ANY"), function (x, i, ..., value) {
    x@.Data[i] <- value
    return(x)
})

##' @rdname Categories
##' @export
setMethod("names", "Categories", function (x) vapply(x, name, character(1)))

##' @rdname Categories
##' @export
setMethod("values", "Categories", function (x) vapply(x, value, numeric(1)))

##' @rdname Categories
##' @export
setMethod("ids", "Categories", function (x) vapply(x, id, integer(1)))
##' @rdname Categories
##' @export
setMethod("ids", "list", function (x) sapply(x, id)) ## for summaries

setNames <- function (x, value) {
    x[] <- mapply(setName, x, value=value, SIMPLIFY=FALSE)
    return(x)
}
setValues <- function (x, value) {
    x[] <- mapply(setValue, x[], value=value, SIMPLIFY=FALSE)
    return(x)
}

##' @rdname Categories
##' @export
setMethod("names<-", "Categories", setNames)
##' @rdname Categories
##' @export
setMethod("values<-", "Categories", setValues)
##' @rdname Categories
##' @export
setMethod("ids<-", "Categories", function (x, value) {
    if (!identical(ids(x), value)) {
        halt("Cannot modify category ids")
    }
    return(x)
})

##' toJSON methods for Crunch objects
##' 
##' @param x the object
##' @param container argument to the generic, not used
##' @param collapse argument to the generic, not used
##' @param .level argument to the generic, not used
##' @param .withNames argument to the generic, not used
##' @param .na argument to the generic, not used
##' @param .escapeEscapes argument to the generic, not used
##' @param pretty argument to the generic, not used
##' @param asIs argument to the generic, not used
##' @param .inf argument to the generic, not used
##' @param ... additional arguments argument to the generic, not used
##' @return JSON-serialized character object
##' @name tojson-crunch
NULL

##' @rdname tojson-crunch
##' @export
setMethod("toJSON", "Categories", function (x, ...) toJSON(I(x@.Data)))

.na.omit.categories <- function (object, ...) {
    missings <- vapply(object, function (x) isTRUE(x$missing), logical(1),
        USE.NAMES=FALSE)
    if (any(missings)) {
        object <- object[!missings]
        attr(object, "na.action") <- which(missings)
        attr(object, "class") <- "omit"
    }
    return(object)
}

##' Omit missing categories
##' @param object Categories
##' @param ... additional arguments, ignored
##' @return \code{object} with any categories that have missing: TRUE excluded
##' @name na-omit-categories
NULL

##' @rdname na-omit-categories
##' @export
setMethod("na.omit", "Categories", function (object, ...) {
    Categories(.na.omit.categories(object))
})

##' is.na for Categories
##'
##' @param x Categories or a single Category
##' @param value To change the missingness of categories, supply either (1)
##' a logical vector of equal length of the categories (or length 1 for the 
##' Category method), or (2) the names of the categories to mark as missing. 
##' If supplying the latter, any categories already indicated as missing will
##' remain missing.
##' @return Getters return logical, a named vector in the case of the Categories
##' method; setters return \code{x} duly modified.
##' @name is-na-categories
NULL

##' @rdname is-na-categories
##' @aliases is-na-categories
##' @export
setMethod("is.na", "Categories", function (x) structure(vapply(x, is.na, logical(1), USE.NAMES=FALSE), .Names=names(x)))

n2i <- function (x, cats) {
    ## Convert x from category names to the corresponding category ids
    if (is.variable(cats)) cats <- categories(cats)
    return(ids(cats)[match(x, names(cats))])
}

##' @rdname is-na-categories
##' @export
setMethod("is.na<-", c("Categories", "character"), function (x, value) {
    ix <- match(value, names(x))
    if (any(is.na(ix))) {
        halt(ifelse(sum(is.na(ix)) > 1, "Categories", "Category"), 
            " not found: ", serialPaste(dQuote(value[is.na(ix)])))
    }
    x[ix] <- lapply(x[ix], `is.na<-`, value=TRUE)
    return(x)
})

##' @rdname is-na-categories
##' @export
setMethod("is.na<-", c("Categories", "logical"), function (x, value) {
    stopifnot(length(x) == length(value))
    x@.Data <- mapply(function (x, value) {
            is.na(x) <- value
            return(x)
        }, x=x@.Data, value=value, USE.NAMES=FALSE, SIMPLIFY=FALSE)
    return(x)
})
