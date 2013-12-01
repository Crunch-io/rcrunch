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

setGeneric("name", function (x) standardGeneric("name"))
setMethod("name", "CrunchDataset", function (x) x@body$name)
setGeneric("name<-", function (x, value) standardGeneric("name<-"),
    signature="x")
setMethod("name<-", "CrunchDataset", setDatasetName)

setGeneric("description", function (x) standardGeneric("description"))
setMethod("description", "CrunchDataset", function (x) x@body$description)
setGeneric("description<-", 
    function (x, value) standardGeneric("description<-"), signature="x")
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

setMethod("[", c("CrunchDataset", "ANY"), function (x, i, ..., drop=FALSE) {
    x@.Data <- x@.Data[i]
    readonly(x) <- TRUE ## we don't want to overwrite the big object accidentally
    return(x)
})
setMethod("[", c("CrunchDataset", "character"), function (x, i, ..., drop=FALSE) {
    i <- names(x) %in% i
    callNextMethod(x, i, ..., drop=drop)
})

showCrunchDataset <- function (x) {
    n <- sQuote(name(x))
    out <- c("Dataset", n, "")
    if (!is.null(x@body$description)) {
        out <- c(out, x@body$description, "")
    }
    
    out <- c(out, 
            "", 
            "Contains", length(x), "variables:", "",
            "")
    vars <- vapply(na.omit(names(x)), function (i) {
        ### REMOVE THE NA.OMIT
        header <- paste0("$", i, ":")
        paste(c(header, getNameAndType(x[[i]]), "\n"), collapse=" ")
    }, character(1))
    out <- c(out, vars)
    
    return(doc(out))
}

setMethod("show", "CrunchDataset", function (object) {
    out <- showCrunchDataset(object)
    cat(out)
    invisible(out)
})
