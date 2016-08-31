#' Hide and Unhide Variables
#' @param x a Variable or subset of a VariableCatalog to hide or unhide
#' @return (invisibly) the Variable or VariableCatalog, hidden or unhidden
#' @name hide
#' @aliases hide unhide
NULL

#' @rdname hide
#' @export
setMethod("hide", "CrunchVariable", function (x) {
    invisible(setTupleSlot(x, "discarded", TRUE))
})
#' @rdname hide
#' @export
setMethod("hide", "VariableCatalog", function (x) {
    invisible(setIndexSlot(x, "discarded", TRUE))
})

#' @rdname hide
#' @export
setMethod("unhide", "CrunchVariable", function (x) {
    invisible(setTupleSlot(x, "discarded", FALSE))
})
#' @rdname hide
#' @export
setMethod("unhide", "VariableCatalog", function (x) {
    invisible(setIndexSlot(x, "discarded", FALSE))
})

#' Hide and Unhide Variables Within a Dataset
#' @param dataset the Dataset to modify
#' @param x same as \code{dataset}, for `hiddenVariables<-`
#' @param variables names or indices of variables to (un)hide
#' @param value same as \code{variables}, for `hiddenVariables<-`
#' @return (invisibly) \code{dataset} with the specified variables (un)hidden
#' @seealso \code{\link{hide}}
#' @export
hideVariables <- function (dataset, variables) {
    var.urls <- urls(allVariables(dataset[variables]))
    allVariables(dataset)[var.urls] <- hide(allVariables(dataset)[var.urls])
    invisible(dataset)
}

#' @rdname hideVariables
#' @export
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

#' @rdname hideVariables
#' @export
unhideVariables <- function (dataset, variables) {
    var.urls <- suppressWarnings(urls(allVariables(dataset[variables])))
    allVariables(dataset)[var.urls] <- unhide(allVariables(dataset)[var.urls])
    invisible(dataset)
}

#' Show the names of hidden variables within the dataset
#' @param dataset the Dataset
#' @param key the Variable attribute to return. Default is "alias", following
#' \code{getOption("crunch.namekey.dataset")}.
#' @return a vector of the names of Variables marked as hidden.
#' @export
hiddenVariables <- function (dataset, key=namekey(dataset)) {
    hv <- hidden(dataset)
    if (length(hv)) {
        return(sort(vapply(index(hv), function (x) x[[key]], character(1),
            USE.NAMES=FALSE)))
    } else {
        return(c())
    }
}
