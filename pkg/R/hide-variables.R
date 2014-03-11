## Variable methods

##' Hide and Unhide Variables
##' @param x a Variable to hide or unhide
##' @return (invisibly) the Variable, hidden or unhidden
##' @export
setMethod("hide", "CrunchVariable", function (x) {
    invisible(setCrunchSlot(x, "discarded", TRUE))
})

##' @rdname hide
##' @export
setMethod("unhide", "CrunchVariable", function (x) {
    invisible(setCrunchSlot(x, "discarded", FALSE))
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
hideVariables <- function (dataset, variables, pattern=NULL, key=namekey(dataset), ...) {
    if (!is.null(pattern)) {
        variables <- findVariables(dataset, pattern=pattern, key=key, ...)
    }
    lapply(dataset[variables], function (x) hide(x))
    invisible(refresh(dataset))
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
unhideVariables <- function (dataset, variables, pattern=NULL,
                            key=namekey(dataset), ...) {
    hidden.vars <- hiddenVariablesList(dataset)
    if (!is.null(pattern)) {
        variables <- findVariables(hidden.vars, pattern=pattern, key=key, ..., value=FALSE)
    } else if (is.character(variables)) {
        ## This should really be a variable collection [ method
        variables <- selectFrom(key, hidden.vars) %in% variables
    }
    lapply(names(hidden.vars[variables]), function (x) unhide(as.variable(GET(x))))
    invisible(refresh(dataset))
}

##' The list of hidden variables
##' @param dataset the Dataset
##' @return a list of Variables marked as hidden
hiddenVariablesList <- function (dataset) {
    return(dataset@hiddenVariables)
}

##' Show the names of hidden variables within the dataset
##' @param dataset the Dataset
##' @return a vector of the names of Variables marked as hidden.
##' @export
hiddenVariables <- function (dataset) {
    hv <- hiddenVariablesList(dataset)
    if (length(hv)) {
        return(vapply(hv, function (x) x$name, character(1), USE.NAMES=FALSE))
    } else {
        return(c())
    }
}

