#' Add columns from one dataset to another, joining on a key
#'
#' As [base::merge()] does for `data.frame`s, this function takes two datasets,
#' matches rows based on a specified key variable, and adds columns from one to
#' the other.
#'
#' Since joining two datasets can sometimes produce unexpected results if the
#' keys differ between the two datasets, you may want to follow the
#' fork-edit-merge workflow for this operation. To do this, fork the dataset
#' with [forkDataset()], join the new data to the fork, ensure that
#' the resulting dataset is correct, and merge it back to the original dataset
#' with [mergeFork()]. For more, see
#' `vignette("fork-and-merge", package = "crunch")`.
#'
#' @param x CrunchDataset to add data to
#' @param y CrunchDataset to copy data from. May be filtered by rows and/or
#' columns.
#' @param by character, optional shortcut for specifying `by.x` and
#' `by.y` by alias if the key variables have the same alias in both
#' datasets.
#' @param by.x CrunchVariable in `x` on which to join, or the alias
#' (following `crunch.namekey.dataset` of a variable. Must be type
#' numeric or text and have all unique, non-missing values.
#' @param by.y CrunchVariable in `y` on which to join, or the alias
#' (following `crunch.namekey.dataset` of a variable. Must be type
#' numeric or text and have all unique, non-missing values.
#' @param all logical: should all rows in x and y be kept, i.e. a "full outer"
#' join? Only `FALSE` is currently supported.
#' @param all.x logical: should all rows in x be kept, i.e. a "left outer"
#' join? Only `TRUE` is currently supported.
#' @param all.y logical: should all rows in y be kept, i.e. a "right outer"
#' join? Only `FALSE` is currently supported.
#' @param copy logical: make a virtual or materialized join. Default is
#' `TRUE`, which means materialized. Virtual joins are in fact not currently
#' implemented, so the default is the only valid value.
#' @param ... additional arguments, ignored
#' @return `x` extended by the columns of `y`, matched on the "by" variables.
#' @export
joinDatasets <- function(x, y, by = intersect(names(x), names(y)), by.x = by,
                         by.y = by, all = FALSE, all.x = TRUE, all.y = FALSE, copy = TRUE) {
    if (copy) {
        ## Just another way to call extend/merge
        Call <- match.call()
        Call[[1L]] <- as.name("extendDataset")
        return(eval.parent(Call))
    }

    ## Else:
    halt("Virtual joins are not yet supported.") # But, someday...
}

#' @rdname joinDatasets
#' @export
extendDataset <- function(x, y, by = intersect(names(x), names(y)), by.x = by, by.y = by,
                          all = FALSE, all.x = TRUE, all.y = FALSE, ...) {

    ## Validate inputs
    by.x <- getJoinByVariable(x, by.x, "x")
    by.y <- getJoinByVariable(y, by.y, "y")
    if (all) halt('Option "all" not supported.')
    if (!all.x) halt('Option "all.x=FALSE" not supported.')
    if (all.y) halt('Option "all.y" not supported.')

    payload <- zfunc("adapt", list(dataset = self(y)), zcl(by.y), zcl(by.x))
    vars <- variablesFilter(y)
    if (!is.null(vars)) {
        payload <- modifyList(vars, list(frame = payload))
    }
    payload$filter <- zcl(activeFilter(y)) ## Effectively not added if NULL
    crPOST(shojiURL(x, "catalogs", "variables"), body = toJSON(payload))
    invisible(refresh(x))
}

getJoinByVariable <- function(dataset, by, name) {
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
