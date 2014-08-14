## Variable methods

##' Hide and Unhide Variables
##' @param x a Variable to hide or unhide
##' @return (invisibly) the Variable, hidden or unhidden
##' @rdname hide
##' @export
setMethod("hide", "CrunchVariable", function (x) {
    invisible(setTupleSlot(x, "discarded", TRUE))
})
setMethod("hide", "VariableCatalog", function (x) {
    invisible(setIndexSlot(x, "discarded", TRUE))
})

##' @rdname hide
##' @export
setMethod("unhide", "CrunchVariable", function (x) {
    invisible(setTupleSlot(x, "discarded", FALSE))
})
setMethod("unhide", "VariableCatalog", function (x) {
    invisible(setIndexSlot(x, "discarded", FALSE))
})

## Dataset methods

##' Hide and Unhide Variables Within a Dataset
##' @param dataset the Dataset to modify
##' @param x same as \code{dataset}, for `hiddenVariables<-`
##' @param variables names or indices of variables to (un)hide
##' @param value same as \code{variables}, for `hiddenVariables<-`
##' @param pattern optional regular expression to identify Variables to (un)hide
##' @param key the Variable attribute to \code{\link{grep}} with the
##' \code{pattern}. Default is "alias"
##' @param ... optional additional arguments to \code{grep}
##' @return (invisibly) \code{dataset} with the specified variables (un)hidden
##' @export
hideVariables <- function (dataset, variables=NULL, pattern=NULL, key=namekey(dataset), ...) {
    var.urls <- findVariableURLs(dataset, refs=variables, pattern=pattern, key=key, ...)
    dataset@variables[var.urls] <- hide(dataset@variables[var.urls])
    invisible(dataset)
}

##' @rdname hideVariables
##' @export
`hiddenVariables<-` <- function (x, value) {
    if (is.character(value)) {
        value <- na.omit(match(value, names(x)))
    }
    if (length(value)) {
        return(hideVariables(x, value))
    } else {
        return(x)
    }
}

##' @rdname hideVariables
##' @export
unhideVariables <- function (dataset, variables=NULL, pattern=NULL,
                            key=namekey(dataset), ...) {
    var.urls <- findVariableURLs(hidden(dataset), refs=variables, pattern=pattern, key=key, ...)
    dataset@variables[var.urls] <- unhide(dataset@variables[var.urls])
    invisible(dataset)
}

setMethod("hidden", "CrunchDataset", function (x) hidden(x@variables))

##' Show the names of hidden variables within the dataset
##' @param dataset the Dataset
##' @return a vector of the names of Variables marked as hidden.
##' @export
hiddenVariables <- function (dataset) {
    hv <- hidden(dataset)
    if (length(hv)) {
        return(sort(vapply(hv@index, function (x) x$name, character(1),
            USE.NAMES=FALSE)))
    } else {
        return(c())
    }
}

