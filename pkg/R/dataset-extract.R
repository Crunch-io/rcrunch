## Things that get

##' @export
setMethod("[", c("CrunchDataset", "ANY"), function (x, i, ..., drop=FALSE) {
    x@variables <- variables(x)[i]
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
    out <- variables(x)[[i]]
    if (!is.null(out)) {
        out <- try(entity(out), silent=TRUE)
        if (is.error(out)) {
            stop(attr(out, "condition")$message, call.=FALSE)
        }
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


## Things that set

.addVariableSetter <- function (x, i, value) {
    if (i %in% names(x)) {
        return(.updateValues(x, i, value))
    } else {
        addVariable(x, values=value, name=i, alias=i)
    }
}

.updateValues <- function (x, i, value, filter=NULL) {
    if (length(i) != 1) {
        stop("Can only update one variable at a time (for the moment)",
            call.=FALSE)
    }
    variable <- x[[i]]
    if (is.null(filter)) {
        variable[] <- value
    } else {
        variable[filter] <- value
    }
    return(x)
}

.updateVariableMetadata <- function (x, i, value) {
    ## Confirm that i matches namekey(value)
    if (i != tuple(value)[[namekey(x)]]) {
        stop("Cannot overwrite one Variable with another", call.=FALSE)
    }
    x@variables[[self(value)]] <- value
    return(x)
}

##' @export
setMethod("[[<-", c("CrunchDataset", "character", "missing", "CrunchVariable"), 
    .updateVariableMetadata)
setMethod("[[<-", c("CrunchDataset", "ANY", "missing", "CrunchVariable"), 
    function (x, i, value) .updateVariableMetadata(x, names(x)[i], value))
setMethod("[[<-", c("CrunchDataset", "character", "missing", "ANY"), .addVariableSetter)
setMethod("[[<-", c("CrunchDataset", "ANY"), function (x, i, value) {
    stop("Only character (name) indexing supported for [[<-", call.=FALSE)
})
##' @export
setMethod("$<-", c("CrunchDataset"), function (x, name, value) {
    x[[name]] <- value
    return(x)
})

setMethod("[<-", c("CrunchDataset", "ANY", "missing", "list"), 
    function (x, i, j, value) {
        ## For lapplying over variables to edit metadata
        stopifnot(length(i) == length(value), 
            all(vapply(value, is.variable, logical(1))))
        for (z in seq_along(i)) {
            x[[i[z]]] <- value[[z]]
        }
        return(x)
    })

## TODO: add similar [<-.CrunchDataset, CrunchDataset/VariableCatalog

setMethod("[<-", c("CrunchDataset", "CrunchExpression", "ANY", "ANY"),
     function (x, i, j, value) {
        if (j %in% names(x)) {
            return(.updateValues(x, j, value, filter=i))
        } else {
            stop("Cannot add variable to dataset with a row index specified",
                call.=FALSE)
        }
    })

