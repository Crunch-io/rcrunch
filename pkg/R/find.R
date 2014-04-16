##' Search a Dataset or list of Variables
##'
##' A version of \code{\link{grep}} for Crunch objects
##' @param dataset the Dataset or list of Crunch objects to search
##' @param pattern regular expression, passed to \code{grep}. If "", returns all.
##' @param key the field in the Crunch objects in which to grep
##' @param hidden logical whether hidden variables should be searched. Default is FALSE
##' @param ... additional arguments passed to \code{grep}. If \code{value=TRUE},
##' returns the values of \code{key} where matches are found, not the variables
##' themselves
##' @return indices of the Variables that match the pattern, or the matching
##' key values if value=TRUE is passed to \code{grep}
##' @export
findVariables <- function (dataset, refs=NULL, pattern="", key=namekey(dataset), ...) {
    
    if (is.dataset(dataset)) {
        dataset <- active(dataset@variables)@index
    }
    keys <- selectFrom(key, dataset)
    if (is.null(refs)) {
        matches <- grep(pattern, keys, ...)
    } else {
        matches <- which(keys %in% refs)
    }
    names(matches) <- NULL
    return(matches)
}

findVariableURLs <- function (x, refs=NULL, pattern="", key=namekey(x), ...) {
    if (is.dataset(x)) {
        key
        return(findVariableURLs(active(x@variables), refs=refs, pattern=pattern, key=key,
            ...))
    } else if (inherits(x, "VariableCatalog")) {
        return(findVariableURLs(x@index, refs=refs, pattern=pattern, key=key,
            ...))
    } else if (!is.numeric(refs)) {
        refs <- findVariables(x, refs=refs, pattern=pattern, key=key, ...)
    }
    return(names(x)[refs])
}