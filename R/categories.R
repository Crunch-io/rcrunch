setValidity("Categories", function(object) {
    are.cats <- vapply(object, is.category, logical(1))
    if (!all(are.cats)) {
        badcount <- sum(!are.cats)
        return(paste0(
            "Invalid categories: ", badcount,
            ifelse(badcount > 1,
                " elements are not Crunch category objects.",
                " element is not a Crunch category object."
            )
        ))
    }
    if (any(duplicated(names(object)))) {
        return("Invalid category names: must be unique")
    }
    if (any(duplicated(ids(object)))) {
        return("Invalid category ids: must be unique")
    }
    return(TRUE)
})

is.categories <- function(x) inherits(x, "Categories")

concatenateCategories <- function(...) {
    ## c() S3 method for categories. Dispatch is on ..1
    dots <- list(...)
    iscat <- vapply(dots, is.category, logical(1))
    iscats <- vapply(dots, is.categories, logical(1))
    if (!all(iscat | iscats)) {
        stop("Invalid categories")
    }
    dots[iscat] <- lapply(dots[iscat], function(x) list(x))
    dots[iscats] <- lapply(dots[iscats], function(x) x@.Data)
    return(Categories(data = do.call(c, dots)))
}

#' S3 method to concatenate Categories and Category objects
#'
#' @param ... see \code{\link[base]{c}}
#' @return An object of class \code{\link{Categories}}
#' @name c-categories
#' @export
#' @examples
#' cat.a <- Category(name="First", id=1, numeric_value=1, missing=FALSE)
#' cat.b <- Category(name="Second", id=2)
#' cat.c <- Category(name="Third", id=3, missing=TRUE)
#' cats.1 <- Categories(cat.a, cat.b)
#' identical(cats.1, c(cat.a, cat.b))
#' identical(c(cats.1, cat.c), Categories(cat.a, cat.b, cat.c))
c.Categories <- concatenateCategories

#' @rdname c-categories
#' @export
c.Category <- concatenateCategories

#' @rdname Categories
#' @export
setMethod("[<-", c("Categories", "ANY"), function(x, i, ..., value) {
    x@.Data[i] <- Categories(data = value)
    return(x)
})


#' @rdname Categories
#' @export
setMethod("ids<-", "Categories", function(x, value) {
    if (!identical(ids(x), value)) {
        halt("Cannot modify category ids")
    }
    return(x)
})

.na.omit.categories <- function(object, ...) {
    missings <- vapply(object, function(x) isTRUE(x$missing), logical(1),
        USE.NAMES = FALSE
    )
    if (any(missings)) {
        object <- object[!missings]
        attr(object, "na.action") <- which(missings)
        attr(object, "class") <- "omit"
    }
    return(object)
}

#' Omit missing categories
#' @param object Categories
#' @param ... additional arguments, ignored
#' @return \code{object} with any categories that have missing: TRUE excluded
#' @name na-omit-categories
NULL

#' @rdname na-omit-categories
#' @export
setMethod("na.omit", "Categories", function(object, ...) {
    Categories(data = .na.omit.categories(object))
})

#' is.na for Categories
#'
#' Crunch categorical variables allow you to set multiple categories as missing.
#' For instance, you might have "not answered" and "doesn't know" both coded as
#' missing. This function returns a logical vector of all dataset entries that
#' fall into any of the missing categories. It also allows you to append
#' additional categories to the list of missing categories using the setter.
#'
#' @param x Categories or a single Category
#' @param value To change the missingness of categories, supply either:
#' 1. a logical vector of equal length of the categories (or length 1 for the
#' Category method); or
#' 1. the names of the categories to mark as missing.
#' If supplying the latter, any categories already indicated as missing will
#' remain missing.
#' @return Getters return logical, a named vector in the case of the Categories
#' method; setters return `x` duly modified.
#' @name is-na-categories
NULL

#' is.selected for Categories
#'
#' Crunch Multiple Response variables identify one or more categories as "selected".
#' These methods allow you to get or set which categories should indicate a selection.
#'
#' @param x Categories or a single Category
#' @param value A logical vector indicating whether the category should be selected.
#' For a single category the value should be either `TRUE` or `FALSE` to change the
#' selection status for a `Categories` object, supply a logical vector which is the
#' same length as the number of categories.
#' @return Getters return a logical vector indicating selection status. Setters return
#' the `Categories` or `Category` object, duly modified.
#' @name is-selected-categories
#' @aliases is.selected<-
NULL

setValues <- function(x, value) {
    x[] <- mapply(setValue, x[], value = value, SIMPLIFY = FALSE)
    return(x)
}

#' @rdname Categories
#' @export
setMethod("values", "Categories", function(x) vapply(x, value, numeric(1)))

#' @rdname Categories
#' @export
setMethod("values<-", "Categories", setValues)

#' @rdname is-selected-categories
#' @export
setMethod("is.selected", "Categories", function(x) {
    structure(vapply(x, is.selected, logical(1), USE.NAMES = FALSE), .Names = names(x))
})

#' @rdname is-selected-categories
#' @export
setMethod("is.selected<-", "Categories", function(x, value) {
    if (is.TRUEorFALSE(value)) {
        value <- rep(value, length(x))
    }
    if (length(value) != length(x)) {
        halt(
            "You supplied ", length(value), " logical values for ", length(x),
            " Categories."
        )
    }

    x@.Data <- mapply(function(x, value) {
        is.selected(x) <- value
        return(x)
    }, x = x@.Data, value = value, USE.NAMES = FALSE, SIMPLIFY = FALSE)
    return(x) 
})

#' @rdname is-na-categories
#' @aliases is-na-categories
#' @export
setMethod("is.na", "Categories", function(x) {
    structure(vapply(x, is.na, logical(1), USE.NAMES = FALSE), .Names = names(x))
})

#' @rdname is-na-categories
#' @export
setMethod("is.na<-", c("Categories", "character"), function(x, value) {
    ix <- match(value, names(x))
    out <- handleMissingCategoryLookup(ix, value, strict = TRUE)
    x[ix] <- lapply(x[ix], `is.na<-`, value = TRUE)
    return(x)
})

#' @rdname is-na-categories
#' @export
setMethod("is.na<-", c("Categories", "logical"), function(x, value) {
    stopifnot(length(x) == length(value))
    x@.Data <- mapply(function(x, value) {
        is.na(x) <- value
        return(x)
    }, x = x@.Data, value = value, USE.NAMES = FALSE, SIMPLIFY = FALSE)
    return(x)
})

n2i <- function(x, cats, strict = TRUE) {
    ## Convert x from category names to the corresponding category ids
    out <- ids(cats)[match(x, names(cats))]
    out <- handleMissingCategoryLookup(out, x, strict)
    return(out)
}

i2n <- function(x, cats, strict = TRUE) {
    ## Convert x from category ids to the corresponding category names
    out <- names(cats)[match(x, ids(cats))]
    out <- handleMissingCategoryLookup(out, x, strict)
    return(out)
}

handleMissingCategoryLookup <- function(result, original, strict = TRUE) {
    bad <- is.na(result)
    if (any(bad)) {
        msg <- paste(
            ifelse(sum(bad) > 1, "Categories", "Category"),
            "not found:", serialPaste(dQuote(original[bad]))
        )
        if (strict) {
            ## Break
            halt(msg)
        } else {
            ## Warn and drop
            msg <- paste0(msg, ". Dropping.")
            warning(msg, call. = FALSE)
            result <- na.omit(result)
        }
    }
    return(result)
}

addNoDataCategory <- function(variable) {
    cats <- ensureNoDataCategory(categories(variable))
    if (is.subvariable(variable)) {
        ## Have to point at parent
        crPATCH(absoluteURL("../../", self(variable)),
            body = toJSON(list(categories = cats))
        )
        variable <- refresh(variable)
    } else {
        categories(variable) <- cats
    }
    return(variable)
}

ensureNoDataCategory <- function(cats) {
    if (-1 %in% ids(cats)) {
        # check "No Data"?
        return(cats)
    } else {
        return(c(cats, Category(data = .no.data)))
    }
}

.no.data <- list(
    id = -1L,
    name = "No Data",
    numeric_value = NULL,
    missing = TRUE
)

.selected.cats <- list(
    list(
        id = 1L,
        name = "Selected",
        numeric_value = 1,
        missing = FALSE,
        selected = TRUE
    ),
    list(
        id = 0L,
        name = "Other",
        numeric_value = 0,
        missing = FALSE
    ),
    .no.data
)

is.3vl <- function(cats) {
    ## Infer whether these categories are from a Three-Valued Logic categorical
    ## This is temporarily stricter than we want so that only formerly boolean
    ## types are detected as logical, not MR subvars or other "selected" vars
    if (!is.categories(cats)) {
        cats <- categories(cats)
    }
    return(
        setequal(ids(cats), c(-1, 0, 1)) &&
            setequal(names(cats), c("Selected", "Other", "No Data")) &&
            sum(is.selected(cats)) == 1 &&
            sum(is.na(cats)) == 1
    )
}
