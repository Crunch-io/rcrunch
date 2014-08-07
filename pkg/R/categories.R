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

##' @export
setMethod("[", c("Categories", "ANY"), function (x, i, ...) {
    x@.Data <- x@.Data[i]
    return(x)
})
##' @export
setMethod("[<-", c("Categories", "ANY"), function (x, i, ..., value) {
    x@.Data[i] <- value
    return(x)
})
##' @export
setMethod("names", "Categories", function (x) vapply(x, name, character(1)))
##' @export
setMethod("values", "Categories", function (x) vapply(x, value, numeric(1)))
##' @export
setMethod("ids", "Categories", function (x) vapply(x, id, numeric(1)))
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

##' @export
setMethod("names<-", "Categories", setNames)
##' @export
setMethod("values<-", "Categories", setValues)
##' @export
setMethod("toJSON", "Categories", function (x, ...) toJSON(I(x@.Data)))

showCategories <- function (x) {
    vapply(x, showCategory, character(1))
}

##' @export
setMethod("show", "Categories", function (object) {
    out <- showCategories(object)
    cat(out, sep="\n")
    invisible(out)
})

##' @export
setMethod("is.dichotomized", "Categories", function (x) any(vapply(x, is.selected, logical(1))))

##' Internal method for dichtomizing Categories (or lists)
##' @rdname dichotomize-internal
##' @param x Categories
##' @param i valid indices for x
##' @return Categories appropriately dichotomized
.dichotomize.categories <- function (x, i) {
    x[i] <- lapply(x[i], function (a) {
        a$selected <- TRUE
        return(a)
    })
    return(x)
}

##' @export
setMethod("dichotomize", c("Categories", "numeric"), .dichotomize.categories)
##' @export
setMethod("dichotomize", c("Categories", "logical"), .dichotomize.categories)
##' @export
setMethod("dichotomize", c("Categories", "character"), function (x, i) {
    ind <- names(x) %in% i
    if (!any(ind)) {
        stop("Category not found") ## make nicer error message
    }
    return(dichotomize(x, ind))
})

##' @export
setMethod("undichotomize", "Categories", function (x) {
    x[] <- lapply(x[], function (a) {
        a$selected <- FALSE
        return(a)
    })
    return(x)
})

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

##' @export
setMethod("na.omit", "Categories", function (object, ...) {
    Categories(.na.omit.categories(object))
})

##' @export
setMethod("is.na", "Categories", function (x) structure(vapply(x, is.na, logical(1), USE.NAMES=FALSE), .Names=names(x)))

n2i <- function (x, cats) {
    ## Convert x from category names to the corresponding category ids
    if (is.variable(cats)) cats <- categories(cats)
    return(ids(cats)[match(x, names(cats))])
}
