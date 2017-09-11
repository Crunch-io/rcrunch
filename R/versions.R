setMethod("initialize", "VersionCatalog", function (.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    ord <- order(from8601(getIndexSlot(.Object, "creation_time")),
        decreasing=TRUE)
    .Object@index <- .Object@index[ord]
    return(.Object)
})

#' Access the saved versions of a dataset
#'
#' This function allows you to see a dataset's savepoints. These can then
#' be passed to [restoreVersion()] to load the previously saved version of a dataset.
#'
#' @param x a `CrunchDataset`
#' @return an object of class `VersionCatalog`. Supported methods on the
#' catalog include "names" and "timestamps".
#' @seealso [saveVersion] [restoreVersion]
#' @export
versions <- function (x) {
    stopifnot(is.dataset(x))
    return(VersionCatalog(crGET(shojiURL(x, "catalogs", "savepoints"))))
}

#' @rdname describe-catalog
#' @export
setMethod("names", "VersionCatalog", function (x) getIndexSlot(x, "description"))

#' @rdname describe-catalog
#' @export
setMethod("descriptions", "VersionCatalog", function (x) getIndexSlot(x, "description"))

#' @rdname describe-catalog
#' @export
setMethod("timestamps", "VersionCatalog", function (x) from8601(getIndexSlot(x, "creation_time")))

#' Create a new saved version
#'
#' Crunch datasets can be saved and restored using `saveVersion` and [restoreVersion()].
#' Some Crunch functions, such as [appendDataset()] create new savepoints automatically. To
#' see the list of savepoints use [versions()].
#'
#' @param dataset a `CrunchDataset`
#' @param description character name to give the saved version, as in a
#' commit message. You are encouraged, though not strictly required, to give
#' versions unique descriptions.
#' @return invisibly, the URL of the newly created version
#' @seealso [versions] [restoreVersion]
#' @export
saveVersion <- function (dataset, description=paste("Version",
                                              length(versions(dataset)) + 1)) {
    if (!is.character(description) || length(description) != 1) {
        halt(dQuote("description"), " must be a length-1 character vector")
    }
    u <- shojiURL(dataset, "catalogs", "savepoints")
    out <- crPOST(u, body=toJSON(list(description=description)),
        config=config(followlocation=0), ## Don't automatically GET the 303 Location
        status.handlers=list(`303`=function (response) {
            message("No unsaved changes; no new version created.")
        }))
    invisible(dataset)
}

#' Restore a dataset to a previously saved version
#'
#' You can save a version of a dataset using [saveVersion()] which will create a
#' new savepoint for the dataset. Savepoints are also created automatically by
#' certain Crunch functions which make major changes to the dataset. You can
#' get the list of saved versions with the [versions()] function.
#'
#' @param dataset a `CrunchDataset`
#' @param version either the name ("description") of the version to restore to
#' or the integer index of the version, as given by `versions(dataset)`
#' @return `dataset`, rolled back to `version`.
#' @seealso [versions] [saveVersion]
#' @export
restoreVersion <- function (dataset, version) {
    vcat <- versions(dataset)
    if (is.numeric(version)) {
        ## Allow passing by index
        v <- version
    } else {
        v <- which(names(vcat) == version)
        if (length(v) == 0) {
            halt(dQuote(version), " does not match any available versions")
        } else if (length(v) > 1) {
            halt(dQuote(version), " matches more than one version. Cannot restore.")
        }
    }
    revert_url <- index(vcat)[[v]]$revert
    crPOST(revert_url, body=NULL)
    invisible(refresh(dataset))
}
