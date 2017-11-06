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
#' @param dataset A Crunch dataset
#' @param variables A Variable, selection of variables from `dataset`, or any
#' other object that can be moved to a folder. For `mkdir`, `variables` is
#' optional.
#' @param path A character "path" to the folder: either a
#' vector of nested folder names or a single string with nested folders
#' separated by a delimiter ("/" default, configurable via
#' `options(crunch.delimiter)`)
#' @return `dataset`, with the folder at `path` guaranteed to be created and
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
mv <- function (dataset, variables, path) {
    ## _as it turns out_, the behavior converges on the same thing as mkdir.
    mkdir(dataset, path, variables)
}

#' @rdname mv
#' @export
mkdir <- function (dataset, path, variables=NULL) {
    path <- parseFolderPath(path)
    if (is.character(variables)) {
        ## Could be aliases, so extract from dataset
        variables <- dataset[variables]
    } else if (is.variable(variables)) {
        variables <- self(variables)
    } else if (inherits(variables, "VariableGroup")) {
        variables <- list(variables)
    }
    ordering(dataset) <- .mkdir.inner(ordering(dataset), path, variables)
    return(invisible(dataset))
}

#' Find and move variables to a new folder
#'
#' @param variable For `folder`, a Variable to find
#' @return `folder` returns the path to the Variable. `folder<-` returns the
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
folder <- function (variable) {
    locateEntity(variable, ordering(loadDataset(datasetReference(variable))))
}

#' @param x For folder assignment, a Variable, selection of variables in a
#' Dataset, or any other object that can be moved to a folder.
#' @param value For assignment, a character "path" to the folder: either a
#' vector of nested folder names or a single string with nested folders
#' separated by a delimiter ("/" default)
#' @rdname folder
`folder<-` <- function (x, value) {
    ds <- mv(loadDataset(datasetReference(x)), x, value)
    return(x)
}

## TODO:
## * mv and mkdir should take an "after" argument for position within folder
##     (and note that it means they'll need slightly different behavior)
## * dir() and folders() listings
## * generally use mv for moving variables' position within a folder? or some
##     other reordering function (dir<- ?)
## * cd() to return a folder object?
## * obviously, use the new API. Some of this might not be reasonable/feasible
##     with current API.
## * rmdir()? or just delete() for folders?

.mkdir.inner <- function (ord, path, variables=NULL) {
    ## Traverse the paths, creating each along the way if they do not already
    ## exist (i.e. always recursive=TRUE)
    segment <- path[1]
    if (!(segment %in% names(ord))) {
        ## Make the folder
        ord[[segment]] <- list()
    }
    if (identical(segment, path)) {
        ## We're at the final level
        if (!is.null(variables)) {
            ## Move the requested variables there
            ## TODO: for mv, this should be additive
            entities(ord[[segment]]) <- variables
        }
    } else {
        ## Go deeper
        ord[[segment]] <- .mkdir.inner(ord[[segment]], path[-1], variables)
    }
    return(ord)
}

parseFolderPath <- function (path) {
    ## path can be "/" separated, and can change that delimiter with
    ## options(crunch.delimiter="|") or something in case you have real "/"
    if (length(path) == 1) {
        path <- unlist(strsplit(path, getOption("crunch.delimiter", "/"), fixed=TRUE))
    }
    return(path)
}
