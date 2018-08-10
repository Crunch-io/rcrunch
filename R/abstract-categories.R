is.AbstractCategories <- function(x) inherits(x, "AbstractCategories")

setMethod("initialize", "AbstractCategories", function(.Object, ...) {
    # list of object constructors to use (based on the class being initialized)
    # for Categories, use Category
    # for Insertions use Insertion
    Constructor <- list(
        AbstractCategories = AbstractCategory,
        Categories = Category,
        Insertions = Insertion,
        Subtotals = Subtotal,
        Headings = Heading
    )[[class(.Object)]]
    stopifnot(is.function(Constructor))

    .Object@.Data <- lapply(..1, function(x) {
        # only reconstruct if we don't have an AbstractCategory already
        # this allows for Insertions to inclue elements of class: Insertion,
        # Subtotal, and Heading
        if (!is.AbstractCategory(x)) {
            x <- try(Constructor(data = x), silent = TRUE)
        }
        return(x)
    })

    validObject(.Object)
    return(.Object)
})

###############################################################
## Abstract Categories manipulation and subsetting methods
###############################################################

setMethod("lapply", "AbstractCategories", function(X, FUN, ...) {
    X@.Data <- lapply(X@.Data, FUN, ...)
    return(X)
})

#' @rdname Categories
#' @export
setMethod("[", c("AbstractCategories", "ANY"), function(x, i, ...) {
    x@.Data <- x@.Data[i]
    return(x)
})

#' @rdname Categories
#' @export
setMethod("[", c("AbstractCategories", "character"), function(x, i, ...) {
    indices <- match(i, names(x))
    if (any(is.na(indices))) {
        halt("subscript out of bounds: ", serialPaste(i[is.na(indices)]))
    }
    callNextMethod(x, i = indices)
})

#' @rdname Categories
#' @export
setMethod("[", c("AbstractCategories", "numeric"), function(x, i, ...) {
    invalid.indices <- setdiff(abs(i), seq_along(x@.Data))
    if (length(invalid.indices)) {
        halt("subscript out of bounds: ", serialPaste(invalid.indices))
    }
    x@.Data <- x@.Data[i]
    return(x)
})

#' @rdname Categories
#' @export
setMethod("[<-", c("AbstractCategories", "character"), function(x, i, ..., value) {
    indices <- match(i, names(x))
    if (any(is.na(indices))) {
        # if there are no matches, add it on to the end
        indices <- i
    }
    x@.Data[indices] <- value
    return(x)
})

#' @rdname Categories
#' @export
setMethod("[[", c("AbstractCategories", "character"), function(x, i, ...) {
    indices <- match(i, names(x))
    if (any(is.na(indices))) {
        halt("subscript out of bounds: ", serialPaste(i[is.na(indices)]))
    }
    callNextMethod(x, i = indices)
})

#' @rdname Categories
#' @export
setMethod("[[<-", c("AbstractCategories", "character"), function(x, i, ..., value) {
    indices <- match(i, names(x))
    if (any(is.na(indices))) {
        # if there are no matches, add it on to the end
        x@.Data[[i]] <- value
        return(x)
    }
    callNextMethod(x, i = indices, value)
})

# a version of modifyList that doesn't recurse into the AbstractCategories themselves
modifyCats <- function(x, val) {
    stopifnot(is.AbstractCategories(x), is.AbstractCategories(val))
    xnames <- names(x)
    vnames <- names(val)
    vnames <- vnames[nzchar(vnames)]
    for (v in vnames) {
        x[[v]] <- val[[v]]
    }

    return(x)
}

###############################################################
## Abstract Categories named get/set methods
###############################################################

#' @rdname Categories
#' @export
setMethod("names", "AbstractCategories", function(x) {
    n <- vapply(x, name, character(1))
    return(n)
})

#' @rdname Categories
#' @export
setMethod("names<-", "AbstractCategories", function(x, value) {
    if (is.null(value) || !is.character(value)) {
        halt('Names must be of class "character"')
    }
    if (!identical(length(x), length(value))) {
        halt(
            "Invalid names: supplied ", length(value), " names for ",
            length(x), " categories"
        )
    }
    if (any(is.na(value))) {
        halt("Category names must be non-missing")
    }
    x[] <- mapply(setName, x, value = value, SIMPLIFY = FALSE)
    return(x)
})


#' @rdname Categories
#' @export
setMethod("ids", "AbstractCategories", function(x) vapply(x, id, integer(1)))

# TODO: concatenateAbstractCategories
