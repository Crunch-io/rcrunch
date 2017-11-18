#' Functions to manipulate variables' folder structure
#'
#' Variables in Crunch datasets are organized into folders, like in a file
#' system. These functions allow you to create new folders and move objects into
#' folders. Their names, `mv` and `mkdir`, suggest their Unix file utility
#' inspiration.
#'
#' The functions have some differences from the strict behavior of their Unix
#' ancestors. For one, they work recursively, without additional arguments:
#' `mkdir` will make every directory necessary to construct the requested path,
#' even if all parent directories didn't already exist; and `mv` doesn't
#' require that the directory to move to already exist. `mkdir` also takes an
#' optional `variables` argument, which allows you to say "make this directory
#' **and** put these variables in it" in one command. As a result of these
#' behaviors, `mv` and `mkdir` here are essentially the same, just with
#' arguments in reverse order: `mv` is move "variables" to "path", while `mkdir`
#' is create "path" and put "variables" in it.
#'
#' Both functions take "dataset" as the first argument, so they naturally
#' support pipelining (as with the \code{\%>\%} operator).
#'
#' @param x A Crunch dataset or VariableFolder
#' @param variables A Variable, selection of variables from `dataset`, or any
#' other object that can be moved to a folder. For `mkdir`, `variables` is
#' optional.
#' @param path A character "path" to the folder: either a
#' vector of nested folder names or a single string with nested folders
#' separated by a delimiter ("/" default, configurable via
#' `options(crunch.delimiter)`)
#' @return `x`, with the folder at `path` guaranteed to be created and
#' `variables`, if specified, moved into it.
#' @seealso [folder()]
#' @examples
#' \dontrun{
#' ds <- loadDataset("Example survey")
#' ds <- mv(ds, c("gender", "age", "educ"), "Demographics")
#' ds <- mkdir(ds, "Key Performance Indicators/Brand X")
#' # These can also be chained together
#' require(magrittr)
#' ds <- ds %>%
#'     mv(c("aware_x", "nps_x"), "Key Performance Indicators/Brand X") %>%
#'     mv(c("aware_y", "nps_y"), "Key Performance Indicators/Brand Y")
#' }
#' @export
mv <- function (x, variables, path) {
    ## TODO: add an "after" argument, pass to addToFolder
    if (is.character(variables)) {
        ## TODO: extract from a folder, including with glob expressions? (*)
        variables <- x[variables]
    }
    f <- cd(x, path, create=TRUE)
    .moveToFolder(f, variables)
    return(invisible(x))
}

#' @rdname mv
#' @export
mkdir <- function (x, path) {
    ## TODO: add an "after" argument, move created folder there
    f <- cd(x, path, create=TRUE)
    return(invisible(x))
}

cd <- function (x, path, create=FALSE) {
    if (is.folder(path)) {
        ## Great! It's already the folder we wanted.
        return(path)
    }
    if (!is.folder(x)) {
        x <- folders(x)
    }
    out <- x[[path, create=create]]
    if (!inherits(out, "ShojiFolder")) {
        halt(deparse(path), " is not a folder")
    }
    return(invisible(out))
}

#' Find and move variables to a new folder
#'
#' @param x For `folder`, a Variable to find. For folder assignment, a Variable, selection of variables in a
#' Dataset, or any other object that can be moved to a folder.
#' @return `folder` returns the parent folder of `x`, or `NULL` if the `x` is the root level. `folder<-` returns the
#' `x` input, having been moved to the requested location.
#' @export
#' @examples
#' \dontrun{
#' ds <- loadDataset("Example survey")
#' folder(ds$income) <- "Demographics/Economic"
#' folder(ds$income)
#' ## [1] "Demographics"    "Economic"
#' }
#' @seealso [mv()]
folder <- function (x) {
    ## NOTE: returns folder, not path. Add a `path()` function
    ## TODO: update man page
    if (is.folder(x)) {
        ## Parent folder is of the same type
        cls <- class(x)
    } else if (is.variable(x)) {
        cls <- "VariableFolder"
    } else {
        halt("No folder for object of class ", class(x))
    }
    u <- tryCatch(shojiURL(x, "catalogs", "folder"),
        error=function (e) return(NULL))
    if (is.null(u)) {
        ## This has no parent. Root level, right?
        return(NULL)
    }
    return(new(cls, crGET(u)))
}

#' @param value For assignment, a character "path" to the folder: either a
#' vector of nested folder names or a single string with nested folders
#' separated by a delimiter ("/" default)
#' @rdname folder
`folder<-` <- function (x, value) {
    if (is.character(value)) {
        ## Turn path into folder
        ## TODO: should this be relative to folder(x) or assumed absolute?
    }
    .moveToFolder(value, x)
    return(x)
}

.moveToFolder <- function (folder, variables) {
    if (is.dataset(variables)) {
        variables <- allVariables(variables)
    }
    if (inherits(variables, "ShojiCatalog")) {
        variables <- urls(variables)
    }
    ## No need to include vars that already exist in this folder
    variables <- setdiff(variables, urls(folder))
    if (length(variables)) {
        ind <- sapply(variables, emptyObject, simplify=FALSE)
        crPATCH(self(folder), body=toJSON(wrapCatalog(index=ind)))
        ## Additional cache invalidation
        ## Drop all variable entities because their catalogs.folder refs are stale
        dropOnly(variables)
        ## TODO: drop all folders
        folder <- refresh(folder)
    }
    return(invisible(folder))
}

## TODO:
## * print method for folders
## * order entities within a folder (including e.g. mv(list of refs, ".")) ?
## * star/regex/filter methods for folders (for e.g. mv("folder A/*", "folder B"))
## * path() and path<-?
## * dir() or something to get contents of a folder?
## * rmdir()? or just delete() for folders?
