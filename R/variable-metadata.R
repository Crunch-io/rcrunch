#' Get all variable metadata for a dataset
#'
#' @param dataset CrunchDataset
#' @return A VariableCatalog that has things like categories embedded in each
#' categorical variable, and all subvariables are represented
#' @export
variableMetadata <- function (dataset) {
    ## 1) Start with the variables catalog
    varcat <- allVariables(dataset)
    ## Absolutize the subvariable URLs, which are only absolutized in normal
    ## use when accessed.
    index(varcat) <- lapply(index(varcat), function (x) {
        if ("subvariables" %in% names(x)) {
            x$subvariables <- absoluteURL(unlist(x$subvariables), self(varcat))
        }
        return(x)
    })

    ## 2) Now supplement that with the "table" metadata, which will give us
    ## a) categories
    ## b) entries for all subvariables
    extra <- crGET(shojiURL(dataset, "fragments", "table"))$metadata
    ## It is keyed by "id". Embed the id in the tuples, and then make the keys
    ## be URLs so that it lines up with the variables catalog
    extra <- mapply(function (x, i) {
            x$id <- i
            return(x)
        }, x=extra, i=names(extra), SIMPLIFY=FALSE)
    names(extra) <- absoluteURL(paste0(names(extra), "/"), self(varcat))

    ## 3) Check to see if `extra` has any subvariables at top level. If not,
    ## either there are no array variables in the dataset, or we're in the
    ## future.
    subvar.urls <- setdiff(names(extra), urls(varcat))
    if (length(subvar.urls)) {
        ## Let's migrate the data to the future shape
        ## Construct subreferences and drop subvars from top level
        ind <- index(varcat)
        extra <- sapply(urls(varcat), function (u) {
            this <- ind[[u]]
            these.subs <- this$subvariables
            if (!is.null(these.subs)) {
                ## Pull in subreferences
                this$subreferences <- lapply(extra[these.subs],
                    function (s) s[intersect(c("name", "alias", "description"), names(s))])
                names(this$subreferences) <- NULL
            }
            ## Merge the entries from the catalog and table
            return(modifyList(extra[[u]], this))
        }, simplify=FALSE)
    } else {
        ## Just merge the entries
        extra <- modifyList(extra, index(varcat))
    }

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
