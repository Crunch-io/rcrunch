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
    out@.dim <- getDim(out)
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

as.dataset <- function (x, useAlias=default.useAlias()) {
    out <- as(x, "CrunchDataset")
    out@useAlias <- useAlias
    return(out)
}

##' @export
setMethod("dim", "CrunchDataset", function (x) x@.dim)

getDim <- function (dataset, filtered=TRUE) {
    which.count <- ifelse(isTRUE(filtered), "filtered", "total")
    ## use filtered by default because every other request will take the applied filter
    
    summary_url <- dataset@urls$summary_url
    nrow <- as.integer(round(GET(summary_url)$rows[[which.count]]))
    return(c(nrow, length(dataset)))
}


namekey <- function (dataset) ifelse(dataset@useAlias, "alias", "name")

##' @export
setMethod("names", "CrunchDataset", function (x) {
    findVariables(x, key=namekey(x), value=TRUE)
})

##' @export
setMethod("[", c("CrunchDataset", "ANY"), function (x, i, ..., drop=FALSE) {
    x@.Data <- x@.Data[i]
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

.addVariableSetter <- function (x, i, value) {
    if (is.variable(value)) {
        x@.Data[[i]] <- value
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
findVariables <- function (dataset, pattern="", key=namekey(dataset), ...) {
    keys <- selectFrom(key, lapply(dataset[], function (x) x@body))
    matches <- grep(pattern, keys, ...)
    names(matches) <- NULL
    return(matches)
}

addVariable <- function (dataset, values, ...) {
    new <- length(values)
    old <- getDim(dataset, filtered=FALSE)[1]
    if (new == 1 && old > 1) {
        values <- rep(values, old)
        new <- old
    }
    if (old > 0 && new != old) {
        stop("replacement has ", new, " rows, data has ", old)
    }
    var_url <- POSTNewVariable(dataset@urls$variables_url, 
        toVariable(values, ...))
    dataset <- refresh(dataset) ## would like not to do this
    # variable <- as.variable(GET(var_url))
    # dataset@.Data[[variable@body$alias]] <- variable
    invisible(dataset)
}

POSTNewVariable <- function (collection_url, variable, bind_url=NULL) {
    
    do.POST <- function (x) POST(collection_url, body=toJSON(x, digits=15))
    # test <- function () stop("")
    is.error <- function (x) inherits(x, "try-error")
    
    if (variable$type %in% c("multiple_response", "categorical_array")) {
        ## assumes: array of subvariables included, and if MR, at least one category has selected: TRUE
        ## TODO: make the data import API take array types directly
        variable$type <- NULL
        subvars <- variable$subvariables
        variable$subvariables <- NULL
        
        var_urls <- lapply(subvars, function (x) try(do.POST(x)))
        errs <- vapply(var_urls, is.error, logical(1))
        if (any(errs)) {
            ## Delete subvariables that were added, then raise
            # lapply(var_urls[!errs], function (x) DELETE(x))
            ## (DELETE not yet supported on variables: https://www.pivotaltracker.com/story/show/65806670)
            stop("Subvariables errored on upload", call.=FALSE)
        } else {
            var_urls <- unlist(var_urls)
        }
        variable$bind_url <- bind_url
        variable$variable_urls <- var_urls
        out <- do.call("POSTBindVariables", variable)
        invisible(out)
    } else {
        invisible(do.POST(variable))
    }
}

addVariables <- function (dataset, vars) {
    ## assume data frame
    nvars <- ncol(vars)
    vars_url <- dataset@urls$variables_url
    for (i in seq_len(nvars)) {
        POSTNewVariable(vars_url,
            toVariable(vars[[i]], name=names(vars)[i], alias=names(vars)[i]))
    }
    invisible(refresh(dataset))
}

##' Get the dataset's weight
##' @param x a Dataset
##' @return a Variable if there is a weight, else NULL
##' @export
weight <- function (x) {
    stopifnot(is.dataset(x))
    w <- x@body$weight
    if (!is.null(w)) {
        w <- as.variable(GET(w))
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