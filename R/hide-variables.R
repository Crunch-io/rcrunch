#' @rdname hide
#' @export
setMethod("hiddenFolder", "CrunchDataset", function(x) hiddenFolder(folders(x)))

#' @rdname hide
#' @export
setMethod("hiddenFolder", "VariableCatalog", function(x) hiddenFolder(folders(x)))

#' @rdname hide
#' @export
setMethod("hiddenFolder", "VariableFolder", function(x) {
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
#' There are several ways to assign variables into these categories
#' and access them:
#' - `hiddenVariables()` / `privateVariabiles()` - return a character vector of variables
#'    that are hidden/private. You can assign into the catalog to add variables or
#'    assign to `NULL` to remove all of them.
#' - `hideVariables()` / `privatizeVariables()` - take a character vector of variable aliases
#'    and makes them hidden/private. (`unhideVariables()` / `deprivatizeVariables()` put them
#'    back in the main variable catalog).
#' - `hide()` / `privatize()` - take a `CrunchVariable` or `VariableCatalog` and
#'    make them hidden/private. (`unhide()` / `deprivatize()` put them back in the main
#'    variable catalog).
#' - `hiddenFolder()` / `privateFolder()` - take a dataset and return a directory that
#'   contains the hidden/private variables. This folder is like other `CrunchFolder`s and
#'   so you can use [`mkdir()`] to create subfolders and [`mv()`] to move them in/out.
#'
#' @param x a Variable, VariableCatalog, or dataset to hide/unhide/privatize/deprivatize
#' @name hide
#' @aliases hide unhide privatize deprivatize
NULL

#' @rdname hide
#' @export
setMethod("hide", "CrunchVariable", function(x) {
    .moveToFolder(hiddenFolder(rootFolder(x)), x)
    # TODO: should these refresh?
    invisible(x)
})
#' @rdname hide
#' @export
setMethod("hide", "VariableCatalog", function(x) {
    .moveToFolder(hiddenFolder(rootFolder(x)), x)
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

#' @rdname hide
#' @export
hideVariables <- function(dataset, variables) {
    dataset <- mv(dataset, variables, hiddenFolder(dataset))
    return(invisible(refresh(dataset)))
}

#' @rdname hide
#' @export
`hiddenVariables<-` <- function(x, value) hideVariables(x, value)

#' @rdname hide
#' @export
unhideVariables <- function(dataset, variables) {
    dataset <- mv(dataset, variables, folders(dataset))
    return(invisible(refresh(dataset)))
}

#' @rdname hide
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

