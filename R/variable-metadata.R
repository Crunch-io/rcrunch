#' Get all variable metadata for a dataset
#'
#' @param dataset CrunchDataset
#' @return A VariableCatalog that has things like categories embedded in each
#' categorical variable, and all subvariables are represented
#' @export
variableMetadata <- function (dataset) {
    ## 1) Start with the variables catalog
    varcat <- allVariables(dataset)

    ## 2) Now supplement that with the "table" metadata, which will give us
    ## a) categories
    ## b) entries for all subvariables
    extra <- crGET(shojiURL(dataset, "fragments", "table"))$metadata
    ## It is keyed by "id". Embed the id in the tuples, and then make the keys
    ## be URLs so that it lines up with the variables catalog
    extra <- mapply(function (x, i) {
        x$id <- i
        if (length(x$subvariables)) {
            ## table/ returns ids, not relative URLs, so make them appear to be
            x$subvariables <- absoluteURL(paste0(i, "/subvariables/", unlist(x$subvariables), "/"),
                self(varcat))
        }
        return(x)
    }, x=extra, i=names(extra), SIMPLIFY=FALSE)
    names(extra) <- absoluteURL(paste0(names(extra), "/"), self(varcat))

    ## Merge the entries
    extra <- modifyList(index(varcat), extra)

    index(varcat) <- extra
    return(varcat)
}

flattenVariableMetadata <- function (vm) {
    ## Put subvar entries at the top level, and inject in them a "parent"
    ## indicator. Like how we used to represent metadata. Needed for
    ## compareSubvariables within a dataset because we need to look across
    ## the datasets for all variables that match on alias and could be pulled
    ## into the array on append--we can't just rely on what's inside the current
    ## array definition.

    ind <- index(vm)
    extra <- lapply(urls(vm), function (u) {
        this <- ind[[u]]
        these.subs <- this$subvariables
        if (!is.null(these.subs)) {
            out <- structure(this$subreferences, .Names=these.subs)
            out <- lapply(out, function (x) {
                ## Add the parent ref
                x$parent <- u
                x$parent_alias <- this$alias
                return(x)
            })
            return(out)
        } else {
            return(NULL)
        }
    })

    index(vm) <- c(ind, unlist(extra, recursive=FALSE))
    return(vm)
}
