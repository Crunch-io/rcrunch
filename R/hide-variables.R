#' Hide/Unhide or Privatize/Deprivatize Variables
#'
#' The public folder is the top level folder of all regular public variables.
#' Both hidden and private are hidden from most views in crunch by default.
#' Hidden variables can be accessed by an user, while private variables
#' (and all variables derived from them) are only accessible
#' by users granted "editor" access to the dataset and so can be used to secure
#' personally identifiable information from non-editors of a dataset.
#'
#' There are several ways to assign variables into these categories
#' and access them:
#' - `hideVariables()` / `privatizeVariables()` - take a character vector of variable aliases
#'    and makes them hidden/private. (`unhideVariables()` / `deprivatizeVariables()` put them
#'    back in the main variable catalog).
#' - `hide()` / `privatize()` - take a `CrunchVariable` or `VariableCatalog` and
#'    make them hidden/private. (`unhide()` / `deprivatize()` put them back in the main
#'    variable catalog).
#' - `hiddenFolder()` / `privateFolder()` / `publicFolder()` - take a dataset and return a folder
#'   that contains the public/hidden/private variables. This folder is like other `CrunchFolder`s
#'   and so you can use [`mkdir()`] to create subfolders and [`mv()`] to move them in/out.
#' - `hiddenVariables()` / `privateVariabiles()` - return a character vector of variables
#'    that are hidden/private. You can assign into the catalog to add variables or
#'    assign to `NULL` to remove all of them.
#'
#' @param x a Variable, VariableCatalog, or dataset to hide/unhide/privatize/deprivatize
#' @param dataset A dataset
#' @param value Replacement values for assignment methods.
#' @param variables Variables to change status of
#' @param key (for `hiddenVariables()` / `privateVariables()` the Variable attribute to
#' return. Default is "alias", following `envOrOption("crunch.namekey.dataset")`.
#' @name hide
#' @aliases hide unhide hiddenFolder privatize deprivatize privateFolder
NULL

.firstLevelFolder <- function(x, type) {
    api_type <- ifelse(type == "private", "secure", type)
    # private variables not available to non-editors
    # hidden is not always present for view
    # but public should be available to all
    api_must_work <- type == "public"

    # Get root variables folder (which contains the first levels inside of it
    # but isn't really used directly). NB can't use `rootFolder()` because
    # it goes to the folder that contains the dataset, not the root variables
    # folder.
    if (!is.dataset(x)) {
        x <- ShojiEntity(crGET(datasetReference(x)))
    }
    root <- VariableFolder(crGET(shojiURL(x, "catalogs", "folders")))

    url <- shojiURL(root, "catalogs", api_type, mustWork = api_must_work)
    if (is.null(url)) return(url)

    VariableFolder(crGET(url))
}

.firstLevelFolderMover <- function(type) {
    mover <- function(x) {
        dir <- .firstLevelFolder(x, type)
        if (is.null(dir)) {
            .folderNotFoundError(type)
        }

        .moveToFolder(dir, x)
        # TODO: should these refresh?
        invisible(x)
    }
    return(mover)
}

.folderNotFoundError <- function(type) {
    halt(
        "Could not find a ", type, " directory, this dataset may not support ",
        type, " variables or you may not have access to them."
    )
}

# ---- Public Variables
#' @rdname hide
#' @export
setMethod("publicFolder", "CrunchDataset", function(x) .firstLevelFolder(x, "public"))

#' @rdname hide
#' @export
setMethod("publicFolder", "VariableCatalog", function(x) .firstLevelFolder(x, "public"))

#' @rdname hide
#' @export
setMethod("publicFolder", "VariableFolder", function(x) .firstLevelFolder(x, "public"))


# ---- Hidden Variables ----------------------
#' @rdname hide
#' @export
setMethod("hiddenFolder", "CrunchDataset", function(x) .firstLevelFolder(x, "hidden"))

#' @rdname hide
#' @export
setMethod("hiddenFolder", "VariableCatalog", function(x) .firstLevelFolder(x, "hidden"))

#' @rdname hide
#' @export
setMethod("hiddenFolder", "VariableFolder", function(x) .firstLevelFolder(x, "hidden"))

#' @rdname hide
#' @export
setMethod("hide", "CrunchVariable", .firstLevelFolderMover("hidden"))

#' @rdname hide
#' @export
setMethod("hide", "VariableCatalog", .firstLevelFolderMover("hidden"))

#' @rdname hide
#' @export
setMethod("unhide", "CrunchVariable", .firstLevelFolderMover("public"))

#' @rdname hide
#' @export
setMethod("unhide", "VariableCatalog", .firstLevelFolderMover("public"))

#' @rdname hide
#' @export
hideVariables <- function(dataset, variables) {
    dir <- hiddenFolder(dataset)
    if (is.null(dir)) .folderNotFoundError("hidden")
    dataset <- mv(dataset, variables, dir)
    return(invisible(refresh(dataset)))
}

#' @rdname hide
#' @export
`hiddenVariables<-` <- function(x, value) hideVariables(x, value)

#' @rdname hide
#' @export
unhideVariables <- function(dataset, variables) {
    dataset <- mv(dataset, variables, publicFolder(dataset))
    return(invisible(refresh(dataset)))
}

#' @rdname hide
#' @export
hiddenVariables <- function(dataset, key = namekey(dataset)) {
    if (is.unforcedVariableCatalog(dataset@hiddenVariables)) {
        hv <- getDatasetHiddenVariables(dataset)
    } else {
        hv <- dataset@hiddenVariables
    }

    if (length(hv)) {
        return(sort(vapply(index(hv), vget(key), character(1),
            USE.NAMES = FALSE
        )))
    } else {
        return(c())
    }
}
