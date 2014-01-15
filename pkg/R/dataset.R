validCrunchDataset <- function (object) {
    oname <- object@body$name
    are.vars <- vapply(object, is.variable, logical(1))
    if (!all(are.vars)) {
        badcount <- sum(!are.vars)
        val <- paste0("Invalid dataset ", sQuote(oname), ": ", badcount, 
            ifelse(badcount>1, 
                " elements are not Crunch variable objects.", 
                " element is not a Crunch variable object."))
    } else {
        val <- TRUE  
    }
    return(val)
}
setValidity("CrunchDataset", validCrunchDataset)

##' Is it?
##' @rdname crunch-is
##' @param x an object
##' @return logical
##' @export 
is.dataset <- function (x) inherits(x, "CrunchDataset")

setDatasetName <- function (x, value) setCrunchSlot(x, "name", value)
setDatasetDescription <- function (x, value) {
    setCrunchSlot(x, "description", value)
}

##' @export
setMethod("name", "CrunchDataset", function (x) x@body$name)
##' @export
setMethod("name<-", "CrunchDataset", setDatasetName)
##' @export
setMethod("description", "CrunchDataset", function (x) x@body$description)
##' @export
setMethod("description<-", "CrunchDataset", setDatasetDescription)

.cr.dataset.shojiObject <- function (x, ...) {
    out <- CrunchDataset(x, ...)
    if (length(list(...))==0) {
        vars <- getDatasetVariables(out)
        if (length(vars)) out@.Data <- vars
    }
    return(out)
}

getDatasetVariables <- function (x) {
    if (!is.null(x@urls$all_variables_url)) {
        return(getAllDatasetVariables(x))
    } else {
        return(getDatasetVariablesFromCollection(x))
    }
}

getDatasetVariablesFromCollection <- function (x) {
    urls <- x@urls$variables_url
    if (!is.null(urls)) {
        vars <- getShojiCollection(urls, "body$alias")
        ordering <- order(selectFrom("body$header_order", vars))
        vars <- lapply(vars[ordering], as.variable)
        return(vars)
    } else {
        return(list())
    }
}

getAllDatasetVariables <- function (x) {
    url <- x@urls$all_variables_url
    vars <- GET(url)
    vars <- lapply(vars, function (a) {
        class(a) <- "shoji"
        return(a)
    })
    names(vars) <- selectFrom("body$alias", vars)
    ordering <- order(selectFrom("body$header_order", vars))
    vars <- lapply(vars[ordering], as.variable)
    return(vars)
}

setAs("ShojiObject", "CrunchDataset", 
    function (from) .cr.dataset.shojiObject(from))
setAs("shoji", "CrunchDataset", 
    function (from) as(as.shojiObject(from), "CrunchDataset"))

as.dataset <- function (x) as(x, "CrunchDataset")

##' @export
setMethod("names", "CrunchDataset", function (x) {
    findVariables(x, key="alias", value=TRUE)
})

##' @export
setMethod("[", c("CrunchDataset", "ANY"), function (x, i, ..., drop=FALSE) {
    x@.Data <- x@.Data[i]
    readonly(x) <- TRUE ## we don't want to overwrite the big object accidentally
    return(x)
})
##' @export
setMethod("[", c("CrunchDataset", "character"), function (x, i, ..., drop=FALSE) {
    i <- names(x) %in% i
    callNextMethod(x, i, ..., drop=drop)
})

##' @export
setMethod("dim", "CrunchDataset", function (x) {
    nrow <- as.integer(GET(x@urls$summary_url)$rows$filtered)
    ## use filtered because every other request will take the applied filter
    return(c(nrow, length(x)))
})
##' @export
setMethod("ncol", "CrunchDataset", function (x) length(x))

showCrunchDataset <- function (x) {
    n <- sQuote(name(x))
    out <- c("Dataset", n, "")
    if (!is.null(x@body$description)) {
        out <- c(out, x@body$description, "")
    }
    
    out <- c(out, 
            "", 
            "Contains", nrow(x), "rows of", ncol(x), "variables:", "",
            "")
    vars <- vapply(na.omit(names(x)), function (i) {
        ### REMOVE THE NA.OMIT
        header <- paste0("$", i, ":")
        paste(c(header, getNameAndType(x[[i]]), "\n"), collapse=" ")
    }, character(1))
    out <- c(out, vars)
    
    return(doc(out))
}

##' @export
setMethod("show", "CrunchDataset", function (object) {
    out <- showCrunchDataset(object)
    cat(out)
    invisible(out)
})

##' Search a Dataset or list of Variables
##'
##' A version of \code{\link{grep}} for Crunch objects
##' @param dataset the Dataset or list of Crunch objects to search
##' @param pattern regular expression, passed to \code{grep}. If "", returns all.
##' @param key the field in the Crunch objects in which to grep
##' @param ... additional arguments passed to \code{grep}. If \code{value=TRUE},
##' returns the values of \code{key} where matches are found, not the variables
##' themselves
##' @return indices of the Variables that match the pattern, or the matching
##' key values if value=TRUE is passed to \code{grep}
##' @export
findVariables <- function (dataset, pattern="", key="alias", ...) {
    keys <- selectFrom(key, lapply(dataset[], function (x) x@body))
    matches <- grep(pattern, keys, ...)
    names(matches) <- NULL
    return(matches)
}
