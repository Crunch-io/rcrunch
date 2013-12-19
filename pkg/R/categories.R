validCategories <- function (object) {
    are.cats <- vapply(object, is.category, logical(1))
    if (!all(are.cats)) {
        badcount <- sum(!are.cats)
        val <- paste0("Invalid categories: ", badcount, 
            ifelse(badcount>1, 
                " elements are not Crunch category objects.", 
                " element is not a Crunch category object."))
    } else {
        val <- TRUE  
    }
    return(val)
}
setValidity("Categories", validCategories)

init.Categories <- function (.Object, ...) {
    .Object@.Data <- lapply(..1, Category)
    return(.Object)
}
setMethod("initialize", "Categories", init.Categories)

is.categories <- function (x) inherits(x, "Categories")

setMethod("[", c("Categories", "ANY"), function (x, i, ...) {
    x@.Data <- x@.Data[i]
    return(x)
})
setMethod("[<-", c("Categories", "ANY"), function (x, i, ..., value) {
    x@.Data[i] <- value
    return(x)
})
setMethod("names", "Categories", function (x) vapply(x, name, character(1)))
setMethod("values", "Categories", function (x) vapply(x, value, numeric(1)))
setMethod("ids", "Categories", function (x) sapply(x, id)) ## could assert numeric?
setMethod("ids", "list", function (x) sapply(x, id)) ## for summaries

setNames <- function (x, value) {
    x[] <- mapply(setName, x, value=value, SIMPLIFY=FALSE)
    return(x)
}
setValues <- function (x, value) {
    x[] <- mapply(setValue, x[], value=value, SIMPLIFY=FALSE)
    return(x)
}

setMethod("names<-", "Categories", setNames)
setMethod("values<-", "Categories", setValues)
setMethod("toJSON", "Categories", function (x, ...) toJSON(I(x@.Data)))

showCategories <- function (x) {
    vapply(x, showCategory, character(1))
}

setMethod("show", "Categories", function (object) {
    out <- showCategories(object)
    cat(out, sep="\n")
    invisible(out)
})

setMethod("is.dichotomized", "Categories", function (x) any(vapply(x, is.selected, logical(1))))

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

setMethod("dichotomize", c("Categories", "numeric"), .dichotomize.categories)
setMethod("dichotomize", c("Categories", "logical"), .dichotomize.categories)
setMethod("dichotomize", c("Categories", "character"), function (x, i) {
    ind <- names(x) %in% i
    if (!any(ind)) {
        stop("Category not found") ## make nicer error message
    }
    return(dichotomize(x, ind))
})

setMethod("undichotomize", "Categories", function (x) {
    x[] <- lapply(x[], function (a) {
        a$selected <- FALSE
        return(a)
    })
    return(x)
})
