#' Add columns from one dataset to another, joining on a key
#'
#' As \code{\link[base]{merge}} does for \code{data.frame}s, this function
#' takes two datasets, matches rows based on a specified key variable, and
#' adds columns from one to the other.
#'
#' @param x CrunchDataset to add data to
#' @param y CrunchDataset to copy data from. May be filtered by rows and/or
#' columns.
#' @param by character, optional shortcut for specifying \code{by.x} and
#' \code{by.y} by alias if the key variables have the same alias in both
#' datasets.
#' @param by.x CrunchVariable in \code{x} on which to join, or the alias
#' (following \code{crunch.namekey.dataset}) of a variable. Must be type
#' numeric or text and have all unique, non-missing values.
#' @param by.y CrunchVariable in \code{y} on which to join, or the alias
#' (following \code{crunch.namekey.dataset}) of a variable. Must be type
#' numeric or text and have all unique, non-missing values.
#' @param all logical: should all rows in x and y be kept, i.e. a "full outer"
#' join? Only \code{FALSE} is currently supported.
#' @param all.x logical: should all rows in x be kept, i.e. a "left outer"
#' join? Only \code{TRUE} is currently supported.
#' @param all.y logical: should all rows in y be kept, i.e. a "right outer"
#' join? Only \code{FALSE} is currently supported.
#' @param copy logical: make a virtual or materialized join. Default is
#' \code{TRUE}, which means materialized. Virtual joins are experimental and
#' not advised.
#' @param ... additional arguments, ignored
#' @return \code{x} extended by the columns of \code{y}, matched on the "by"
#' variables.
#' @export
joinDatasets <- function (x, y, by=intersect(names(x), names(y)), by.x=by,
                         by.y=by, all=FALSE, all.x=TRUE, all.y=FALSE, copy=TRUE) {

    if (copy) {
        ## Just another way to call extend/merge
        Call <- match.call()
        Call[[1L]] <- as.name("extendDataset")
        return(eval.parent(Call))
    }

    ## Else:
    warning("Virtual joins are experimental. Use with extreme caution.",
        call.=FALSE)
    ## Validate inputs
    by.x <- getJoinByVariable(x, by.x, "x")
    by.y <- getJoinByVariable(y, by.y, "y")
    if (all) halt('Option "all" not supported.')
    if (!all.x) halt('Option "all.x=FALSE" not supported.')
    if (all.y) halt('Option "all.y" not supported.')
    ## Get join catalog url
    join_url <- shojiURL(x, "catalogs", "joins")

    payload <- structure(list(list(left_key=self(by.x), right_key=self(by.y))),
        .Names=paste0(join_url, tuple(y)$id, "/"))
    crPATCH(join_url, body=toJSON(payload))
    invisible(refresh(x))
}

#' @rdname joinDatasets
#' @export
extendDataset <- function (x, y, by=intersect(names(x), names(y)), by.x=by, by.y=by,
                           all=FALSE, all.x=TRUE, all.y=FALSE, ...) {

    ## Validate inputs
    by.x <- getJoinByVariable(x, by.x, "x")
    by.y <- getJoinByVariable(y, by.y, "y")
    if (all) halt('Option "all" not supported.')
    if (!all.x) halt('Option "all.x=FALSE" not supported.')
    if (all.y) halt('Option "all.y" not supported.')

    y_frame <- list(
        dataset=self(y),
        filter=zcl(activeFilter(y))
    )
    y_frame$select <- variablesFilter(y)
    payload <- zfunc("adapt", y_frame, zcl(by.y), zcl(by.x))
    crPOST(shojiURL(x, "catalogs", "variables"), body=toJSON(payload))
    invisible(refresh(x))
}

getJoinByVariable <- function (dataset, by, name) {
    ## Do validations and return a proper, legal "by" variable, if possible
    if (!is.dataset(dataset)) halt(name, " must be a Crunch Dataset")
    if (is.character(by)) {
        if (length(by) != 1) {
            halt("by.", name, " must reference one and only one variable")
        }
        byvar <- dataset[[by]]
        if (is.null(byvar)) {
            halt(by, " does not reference a variable in ", name)
        }
        by <- byvar
    }
    if (!is.variable(by)) halt("by.", name, " must be a Crunch Variable")
    if (!(self(by) %in% urls(allVariables(dataset)))) {
        halt("by.", name, " must be a variable in ", name)
    }
    if (!(type(by) %in% c("text", "numeric"))) {
        halt("by.", name, " must be type numeric or text")
    }
    return(by)
}

#' @rdname joinDatasets
#' @export
merge.CrunchDataset <- extendDataset
