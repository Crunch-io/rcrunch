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
    hideVariables(x, value)
}

##' @rdname hideVariables
##' @export
unhideVariables <- function (dataset, variables, pattern=NULL, key=namekey(dataset), 
                            ...) {
    hidden.vars <- hiddenVariablesList(dataset)
    if (!is.null(pattern)) {
        variables <- findVariables(hidden.vars, pattern=pattern, key=key, ...)
    }
    lapply(hidden.vars[variables], function (x) unhide(x))
    invisible(refresh(dataset))
}

##' The list of hidden variables
##' @param dataset the Dataset
##' @return a list of Variables marked as hidden
hiddenVariablesList <- function (dataset) {
    vars <- getShojiCollection(dataset@urls$discarded_variables_url,
        "body$alias")
    return(lapply(vars, as.variable))
}

##' Show the names of hidden variables within the dataset
##' @param dataset the Dataset
##' @return a vector of the names of Variables marked as hidden. Vector elements
##' are named by the Variables' aliases.
##' @export
hiddenVariables <- function (dataset) {
    hv <- hiddenVariablesList(dataset)
    if (length(hv)) {
        return(vapply(hv, function (x) name(x), character(1)))
    } else {
        return(c())
    }
}

