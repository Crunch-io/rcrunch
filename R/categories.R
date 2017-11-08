setValidity("Categories", function (object) {
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
})

is.categories <- function (x) inherits(x, "Categories")

concatenateCategories <- function (...) {
    ## c() S3 method for categories. Dispatch is on ..1
    dots <- list(...)
    iscat <- vapply(dots, is.category, logical(1))
    iscats <- vapply(dots, is.categories, logical(1))
    if (!all(iscat | iscats)) {
        stop("Invalid categories")
    }
    dots[iscat] <- lapply(dots[iscat], function (x) list(x))
    dots[iscats] <- lapply(dots[iscats], function (x) x@.Data)
    return(Categories(data=do.call(c, dots)))
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
setMethod("[<-", c("Categories", "ANY"), function (x, i, ..., value) {
    x@.Data[i] <- Categories(data=value)
    return(x)
})


#' @rdname Categories
#' @export
setMethod("ids<-", "Categories", function (x, value) {
    if (!identical(ids(x), value)) {
        halt("Cannot modify category ids")
    }
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

#' Omit missing categories
#' @param object Categories
#' @param ... additional arguments, ignored
#' @return \code{object} with any categories that have missing: TRUE excluded
#' @name na-omit-categories
NULL

#' @rdname na-omit-categories
#' @export
setMethod("na.omit", "Categories", function (object, ...) {
    Categories(data=.na.omit.categories(object))
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

n2i <- function (x, cats, strict=TRUE) {
    ## Convert x from category names to the corresponding category ids
    out <- ids(cats)[match(x, names(cats))]
    out <- handleMissingCategoryLookup(out, x, strict)
    return(out)
}

i2n <- function (x, cats, strict=TRUE) {
    ## Convert x from category ids to the corresponding category names
    out <- names(cats)[match(x, ids(cats))]
    out <- handleMissingCategoryLookup(out, x, strict)
    return(out)
}

handleMissingCategoryLookup <- function (result, original, strict=TRUE) {
    bad <- is.na(result)
    if (any(bad)) {
        msg <- paste(ifelse(sum(bad) > 1, "Categories", "Category"),
            "not found:", serialPaste(dQuote(original[bad])))
        if (strict) {
            ## Break
            halt(msg)
        } else {
            ## Warn and drop
            msg <- paste0(msg, ". Dropping.")
            warning(msg, call.=FALSE)
            result <- na.omit(result)
        }
    }
    return(result)
}

addNoDataCategory <- function (variable) {
    cats <- ensureNoDataCategory(categories(variable))
    if (is.subvariable(variable)) {
        ## Have to point at parent
        crPATCH(absoluteURL("../../", self(variable)),
            body=toJSON(list(categories=cats)))
        variable <- refresh(variable)
    } else {
        categories(variable) <- cats
    }
    return(variable)
}

ensureNoDataCategory <- function (cats) {
    if (-1 %in% ids(cats)) {
        # check "No Data"?
        return(cats)
    } else {
        return(c(cats, Category(data=.no.data)))
    }
}

#' Change the id of a category for a categorical variable
#'
#' Changes the id of a category from an existing value to a new one.
#' The variable can be a categorical, categorical array, or multiple response
#' variable. The category changed will have the same numeric value and missing
#' status as before. The one exception to this is if the numeric value is the
#' same as the id, then the new numeric value will be the same as the new id.
#'
#' @param variable the variable in a crunch dataset that will be changed (note: the variable must be categorical, categorical array, or multiple response)
#' @param from the (old) id identifying the category you want to change
#' @param to the (new) id for the category
#' @return `variable` with category `from` and all associated data values mapped to id `to`
#' @examples
#' \dontrun{
#' ds$country <- changeCategoryID(ds$country, 2, 6)
#' }
#' @export
changeCategoryID <- function (variable, from, to) {
    if (!has.categories(variable)) {
        halt("The variable ", name(variable), " doesn't have categories.")
    }

    if (!is.numeric(from) & length(from) == 1) {
        halt("from should be a single numeric")
    }

    if (!is.numeric(to) &  length(to) == 1) {
        halt("to should be a single numeric")
    }

    pos.from <- match(from, ids(categories(variable)))
    if (is.na(pos.from)) {
        halt("No category with id ", from)
    }

    if (to %in% ids(categories(variable))) {
        halt("Id ", to, " is already a category, please provide a new category id.")
    }

    ## Add new category
    newcat <- categories(variable)[[pos.from]]
    # if the old id matches the old numeric value, likely the user wants these
    # to be the same, so change the new numeric value to be the same as the
    # new id.
    if (newcat$id == newcat$numeric_value %||% FALSE) {
        newcat$numeric_value <- to
    }
    newcat$id <- to

    names(categories(variable))[pos.from] <- "__TO_DELETE__"
    categories(variable) <- c(categories(variable), newcat)

    ## Move data to that new id
    if (is.Categorical(variable)) {
        variable[variable == from] <- to
    } else if (is.Array(variable)) {
        # If the variable is an array, then lapply over the subvariables
        # TODO: change iteration over shojicatalogs to allow iterating over the variable directly
        lapply(names(variable), function (subvarname) {
            variable[[subvarname]][variable[[subvarname]] == from] <- to
        })
    }

    ## Delete old category
    keep <- seq_along(categories(variable))
    keep[pos.from] <- length(keep)
    keep <- keep[-length(keep)]
    categories(variable) <- categories(variable)[keep]

    invisible(variable)
}
