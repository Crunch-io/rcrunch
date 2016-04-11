##' Search a Dataset or list of Variables
##'
##' A version of \code{\link{grep}} for Crunch objects
##' @param dataset the Dataset or list of Crunch objects to search
##' @param refs vector of specific values in \code{key} to look for, as an
##' alternative to pattern matching. Default is \code{NULL}, which means to
##' use \code{grep} instead.
##' @param pattern regular expression, passed to \code{grep}. If "", returns
##' all. Note: this argument is deprecated.
##' @param key the field in the Crunch objects in which to grep. Likewise deprecated.
##' @param ... additional arguments passed to \code{grep}. If \code{value=TRUE},
##' returns the values of \code{key} where matches are found, not the variables
##' themselves. Again, deprecated.
##' @return indices of the Variables that match the pattern, or the matching
##' key values if value=TRUE is passed to \code{grep}
##' @keywords internal
findVariables <- function (dataset, refs=NULL, pattern="", key=namekey(dataset), ...) {

    if (is.dataset(dataset)) {
        dataset <- variables(dataset)
    }
    keys <- getIndexSlot(dataset, key)
    if (is.null(refs)) {
        warning(paste("Deprecation warning: pattern matching in this function",
            "will be removed in a future release. Please reference entities",
            "directly. See the help page for this function for more."),
            call.=FALSE)
        matches <- grep(pattern, keys, ...)
    } else {
        matches <- which(keys %in% refs)
    }
    names(matches) <- NULL
    return(matches)
}

findVariableURLs <- function (x, refs=NULL, pattern="", key=namekey(x), ...) {
    if (inherits(refs, "VariableOrder") || inherits(refs, "VariableGroup")) {
        return(urls(refs))
    } else if (is.dataset(x)) {
        key
        return(findVariableURLs(variables(x), refs=refs, pattern=pattern,
            key=key, ...))
    } else if (!is.numeric(refs)) {
        refs <- findVariables(x, refs=refs, pattern=pattern, key=key, ...)
    }
    return(urls(x)[refs])
}
