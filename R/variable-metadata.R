##' Get all variable metadata for a dataset
##'
##' @param dataset CrunchDataset
##' @param parent logical: Embed in subvariables a reference to their array
##' parent? Default is \code{FALSE}.
##' @return A VariableCatalog that has things like categories embedded in each
##' categorical variable, and all subvariables are represented
##' @export
variableMetadata <- function (dataset, parent=FALSE) {
    ## TODO: make a nested vs. flat version. Current is flat.

    ## Start with the variables catalog
    varcat <- allVariables(dataset)
    ## Absolutize the subvariable URLs, which are only absolutized in normal
    ## use when accessed.
    index(varcat) <- lapply(index(varcat), function (x) {
        if ("subvariables" %in% names(x)) {
            x$subvariables <- absoluteURL(unlist(x$subvariables), self(varcat))
        }
        return(x)
    })
    ## Now supplement that with the "table" metadata, which will give us
    ## 1) categories
    ## 2) entries for all subvariables
    extra <- crGET(shojiURL(dataset, "fragments", "table"))$metadata
    ## It is keyed by "id". Embed the id in the tuples, and then make the keys
    ## be URLs so that it lines up with the variables catalog
    # extra <- mapply(function (x, i) {
    #         x$id <- i
    #         return(x)
    #     }, x=extra, i=names(extra), SIMPLIFY=FALSE)
    names(extra) <- absoluteURL(paste0(names(extra), "/"), self(varcat))
    ## Do a list update to merge the two objects
    extra <- modifyList(extra, index(varcat))

    ## Optionally iterate and add a marker for subvariables' parent vars
    if (parent) {
        for (i in names(extra)) {
            ## Iterate over variables and find arrays
            this <- extra[[i]]
            if (length(this$subvariables)) {
                ## If there are subvariables, poke a "parent" into their tuples
                extra[this$subvariables] <- lapply(extra[this$subvariables],
                    function (v) {
                        v$parent <- this$id
                        return(v)
                    })
            }
        }
    }
    index(varcat) <- extra
    return(varcat)
}

varTable <- function (dataset) {
    ## Make a data.frame from the variables catalog
    return(catalogToDataFrame(variableMetadata(dataset, parent=TRUE),
        c("name", "alias", "parent", "type", "id"), stringsAsFactors=FALSE))
}

are.subvars <- function (vars) {
    vapply(index(vars), function (x) !is.null(x$parent), logical(1))
}
