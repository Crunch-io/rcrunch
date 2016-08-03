findVariableURLs <- function (x, refs=NULL, pattern="", key=namekey(x), ...) {
    if (inherits(refs, "VariableOrder") || inherits(refs, "VariableGroup")) {
        return(urls(refs))
    }
    if (is.dataset(x)) {
        force(key)
        if (is.character(refs)) {
            ## Get allVariables so we'll lookup urls for hidden vars too
            x <- allVariables(x)
        } else {
            x <- variables(x)
        }
    }

    if (!is.numeric(refs)) {
        ## Do the looking up.
        keys <- getIndexSlot(x, key)
        if (is.null(refs)) {
            warning(paste("Deprecation warning: pattern matching in this function",
                "will be removed in a future release. Please reference entities",
                "directly. See the help page for this function for more."),
                call.=FALSE)
            refs <- grep(pattern, keys, ...)
        } else {
            refs <- which(keys %in% refs)
        }
        names(refs) <- NULL
    }
    return(urls(x)[refs])
}
