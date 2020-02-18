setMethod("hidden", "CrunchDataset", function(x) hidden(folders(x)))

setMethod("hidden", "VariableCatalog", function(x) hidden(folders(x)))

setMethod("hidden", "VariableFolder", function(x) {
    return(VariableFolder(crGET(shojiURL(rootFolder(x), "catalogs", "hidden"))))
})

#' Hide/Unhide or Privatize/Deprivatize Variables
#' 
#' Both hidden and private are hidden from most views in crunch by default. 
#' Hidden variables can be accessed by an user, while private variables 
#' (and all variables derived from them) are only accessible 
#' by users granted "editor" access to the dataset and so can be used to secure
#' personally identifiable information from non-editors of a dataset.
#' 
#' @param x a Variable or subset of a VariableCatalog to hide or unhide
#' @return (invisibly) the Variable or VariableCatalog, hidden or unhidden
#' @name hide
#' @aliases hide unhide privatize deprivatize
#' @seealso [`hideVariables`] [`privateVariables`]
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
#' 
#' Both hidden and private are hidden from most views in crunch by default. 
#' Hidden variables can be accessed by an user, while private variables 
#' (and all variables derived from them) are only accessible 
#' by users granted "editor" access to the dataset and so can be used to secure
#' personally identifiable information from non-editors of a dataset.
#' 
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

#' Show the names of a dataset's hidden or private variables
#' 
#' Both hidden and private are hidden from most views in crunch by default. 
#' Hidden variables can be accessed by an user, while private variables 
#' (and all variables derived from them) are only accessible 
#' by users granted "editor" access to the dataset and so can be used to secure
#' personally identifiable information from non-editors of a dataset.
#' 
#' @param dataset the Dataset
#' @param key the Variable attribute to return. Default is "alias", following
#' `getOption("crunch.namekey.dataset")`.
#' @return a vector of the names of Variables marked as hidden/private.
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
