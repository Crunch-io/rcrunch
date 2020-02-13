setMethod("hidden", "CrunchDataset", function(x) hidden(folders(x)))

setMethod("hidden", "VariableCatalog", function(x) hidden(folders(x)))

setMethod("hidden", "VariableFolder", function(x) {
    return(VariableFolder(crGET(shojiURL(rootFolder(x), "catalogs", "hidden"))))
})

#' Hide and Unhide Variables
#' @param x a Variable or subset of a VariableCatalog to hide or unhide
#' @return (invisibly) the Variable or VariableCatalog, hidden or unhidden
#' @name hide
#' @aliases hide unhide
#' @seealso [`hideVariables`]
NULL

#' @rdname hide
#' @export
setMethod("hide", "CrunchVariable", function(x) {
    .moveToFolder(hidden(rootFolder(x)), x)
    # TODO: should these refresh?
    invisible(x)
})
#' @rdname hide
#' @export
setMethod("hide", "VariableCatalog", function(x) {
    .moveToFolder(hidden(rootFolder(x)), x)
    invisible(x)
})

#' @rdname hide
#' @export
setMethod("unhide", "CrunchVariable", function(x) {
    .moveToFolder(rootFolder(x), x)
    invisible(x)
})
#' @rdname hide
#' @export
setMethod("unhide", "VariableCatalog", function(x) {
    .moveToFolder(rootFolder(x), x)
    invisible(x)
})

#' Hide and unhide variables within a dataset
#' @param dataset the Dataset to modify
#' @param x `dataset`, for `hiddenVariables<-`
#' @param variables names or indices of variables to (un)hide
#' @param value `variables`, for `hiddenVariables<-`
#' @return (invisibly) `dataset` with the specified variables (un)hidden
#' @seealso [`hide`]
#' @export
hideVariables <- function(dataset, variables) {
    dataset <- mv(dataset, variables, hidden(dataset))
    return(invisible(refresh(dataset)))
}

#' @rdname hideVariables
#' @export
`hiddenVariables<-` <- function(x, value) hideVariables(x, value)

#' @rdname hideVariables
#' @export
unhideVariables <- function(dataset, variables) {
    dataset <- mv(dataset, variables, folders(dataset))
    return(invisible(refresh(dataset)))
}

#' Show the names of a dataset's hidden variables
#' @param dataset the Dataset
#' @param key the Variable attribute to return. Default is "alias", following
#' `getOption("crunch.namekey.dataset")`.
#' @return a vector of the names of Variables marked as hidden.
#' @export
hiddenVariables <- function(dataset, key = namekey(dataset)) {
    hv <- dataset@hiddenVariables
    if (length(hv)) {
        return(sort(vapply(index(hv), vget(key), character(1),
            USE.NAMES = FALSE
        )))
    } else {
        return(c())
    }
}
