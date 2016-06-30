##' Add columns from one dataset to another, joining on a key
##'
##' As \code{\link[base]{merge}} does for \code{data.frame}s, this function
##' takes two datasets, matches rows based on a specified key variable, and
##' adds columns from one to the other.
##'
##' @param x CrunchDataset to add data to
##' @param y CrunchDataset to copy data from. May be filtered by rows and/or
##' columns.
##' @param by.x CrunchVariable in \code{x} on which to join. Must have all
##' unique, non-missing values.
##' @param by.y CrunchVariable in \code{y} on which to join. Must have all
##' unique, non-missing values.
##' @param all logical: should all rows in x and y be kept, i.e. a "full outer"
##' join? Only \code{FALSE} is currently supported.
##' @param all.x logical: should all rows in x be kept, i.e. a "left outer"
##' join? Only \code{TRUE} is currently supported.
##' @param all.x logical: should all rows in y be kept, i.e. a "right outer"
##' join? Only \code{FALSE} is currently supported.
##' @param ... additional arguments, ignored
##' @return \code{x} extended by the columns of \code{y}, matched on the "by"
##' variables.
##' @export
extendDataset <- function (x, y, by.x, by.y,
                           all=FALSE, all.x=TRUE, all.y=FALSE, ...) {

    ## Validate inputs
    if (!is.dataset(x)) halt("x must be a Crunch Dataset")
    if (!is.dataset(y)) halt("y must be a Crunch Dataset")
    if (!is.variable(by.x)) halt("by.x must be a Crunch Variable")
    if (!(self(by.x) %in% urls(allVariables(x)))) {
        halt("by.x must be a variable in x")
    }
    if (!is.variable(by.y)) halt("by.y must be a Crunch Variable")
    if (!(self(by.y) %in% urls(allVariables(y)))) {
        halt("by.y must be a variable in y")
    }
    if (all) halt('Option "all" not supported.')
    if (!all.x) halt('Option "all.x=FALSE" not supported.')
    if (all.y) halt('Option "all.y" not supported.')

    y_frame <- list(
        dataset=self(y),
        filter=zcl(activeFilter(y))
    )
    y_frame$select <- variablesFilter(y)
    payload <- zfunc("adapt", list(
            y_frame,
            zcl(by.y),
            zcl(by.x)
        ))
    crPOST(shojiURL(x, "catalogs", "variables"), body=toJSON(payload))
    invisible(refresh(x))
}

##' @rdname extendDataset
##' @export
merge.CrunchDataset <- extendDataset
