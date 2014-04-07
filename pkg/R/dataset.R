init.fromShoji <- function (.Object, shoji, ...) {
    slots <- slotNames(.Object)
    if (!missing(shoji) && is.shojiObject(shoji)) {
        for (i in slotNames(shoji)) {
            if (i %in% slots) {
                slot(.Object, i) <- slot(shoji, i)
            }
        }
        dots <- list(...)
        for (i in names(dots)) {
            if (i %in% slots) {
                slot(.Object, i) <- dots[[i]]
            }
        }
    } else {
        .Object <- callNextMethod(.Object, ...)
    }
    return(.Object)
}
setMethod("initialize", "CrunchDataset", init.fromShoji)

validCrunchDataset <- function (object) {
    oname <- object@body$name
    are.vars <- vapply(object@variables, is.variable.tuple, logical(1))
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

.cr.dataset.shojiObject <- function (x) {
    out <- CrunchDataset(shoji=x)
    out@variables <- getDatasetVariables(out)
    out@.nrow <- getNrow(out)
    return(out)
}

getDatasetVariables <- function (x) {
    return(do.call(VariableCatalog, GET(x@urls$variables_url)))
}

setAs("ShojiObject", "CrunchDataset", 
    function (from) .cr.dataset.shojiObject(from))
setAs("shoji", "CrunchDataset", 
    function (from) as(as.shojiObject(from), "CrunchDataset"))

as.dataset <- function (x, useAlias=default.useAlias()) {
    out <- as(x, "CrunchDataset")
    out@useAlias <- useAlias
    return(out)
}

##' @export
setMethod("dim", "CrunchDataset", function (x) c(x@.nrow, length(active(x@variables))))

getNrow <- function (dataset, filtered=TRUE) {
    which.count <- ifelse(isTRUE(filtered), "filtered", "total")
    ## use filtered by default because every other request will take the applied filter
    
    summary_url <- dataset@urls$summary_url
    nrows <- as.integer(round(GET(summary_url)$rows[[which.count]]))
    return(nrows)
}


namekey <- function (dataset) ifelse(dataset@useAlias, "alias", "name")

##' @export
setMethod("names", "CrunchDataset", function (x) {
    findVariables(x, key=namekey(x), value=TRUE)
})

variableNames <- function (x) {
    findVariables(x, key="name", value=TRUE)
}

##' @export
setMethod("[", c("CrunchDataset", "ANY"), function (x, i, ..., drop=FALSE) {
    x@variables <- active(x@variables)[i]
    readonly(x) <- TRUE ## we don't want to overwrite the big object accidentally
    return(x)
})
##' @export
setMethod("[", c("CrunchDataset", "character"), function (x, i, ..., drop=FALSE) {
    w <- match(i, names(x))
    if (any(is.na(w))) {
        stop("Undefined columns selected: ", serialPaste(i[is.na(w)]))
    }
    callNextMethod(x, w, ..., drop=drop)
})
##' @export
setMethod("[[", c("CrunchDataset", "ANY"), function (x, i, ..., drop=FALSE) {
    out <- try(entity(active(x@variables)[[i]]), silent=TRUE)
    if (is.error(out)) {
        stop(attr(out, "condition")$message, call.=FALSE)
    }
    return(out)
})
##' @export
setMethod("[[", c("CrunchDataset", "character"), function (x, i, ..., drop=FALSE) {
    stopifnot(length(i) == 1)
    i <- match(i, names(x))
    if (is.na(i)) return(NULL)
    callNextMethod(x, i, ..., drop=drop)
})
##' @export
setMethod("$", "CrunchDataset", function (x, name) x[[name]])

.addVariableSetter <- function (x, i, value) {
    if (is.variable(value)) {
        ## What do we do with "i"? ## Just confirm that matches namekey(value)?
        x@variables[[self(value)]] <- value
        return(x)
    }
    if (i %in% names(x)) {
        stop("Cannot currently overwrite existing Variables with [[<-",
            call.=FALSE)
    }
    addVariable(x, values=value, name=i, alias=i)
}
##' @export
setMethod("[[<-", c("CrunchDataset", "character"), .addVariableSetter)
setMethod("[[<-", c("CrunchDataset", "ANY"), function (x, i, value) {
    stop("Only character (name) indexing supported for [[<-", call.=FALSE)
})
##' @export
setMethod("$<-", c("CrunchDataset"), function (x, name, value) .addVariableSetter(x, i=name, value))

## TODO: add [<-.CrunchDataset, CrunchDataset/VariableCatalog

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
    ## TODO: update with VariableCatalog
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

##' Get the dataset's weight
##' @param x a Dataset
##' @return a Variable if there is a weight, else NULL
##' @export
weight <- function (x) {
    stopifnot(is.dataset(x))
    w <- x@body$weight
    if (!is.null(w)) {
        w <- entity(x@variables[[w]])
    }
    return(w)
}

##' Set the dataset's weight
##' @param x a Dataset
##' @param value a Variable to set as weight, or NULL to remove the existing weight
##' @return x, modified accordingly
##' @export
`weight<-` <- function (x, value) {
    stopifnot(is.dataset(x))
    if (is.variable(value)) {
        value <- self(value)
    } else if (!is.null(value)) {
        stop("Weight must be a Variable or NULL")
    }
    x <- setCrunchSlot(x, "weight", value)
    return(x)
}

setMethod("lapply", "CrunchDataset", function (X, FUN, ...) {
    vars <- lapply(seq_along(active(X@variables)), function (i) X[[i]])
    callNextMethod(vars, FUN, ...)
})

is.variable.tuple <- function (x) {
    is.list(x) && all(c("name", "alias", "type", "id") %in% names(x))
}