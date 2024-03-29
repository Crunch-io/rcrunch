#' Get and set VariableOrder
#'
#' The `ordering` methods allow you to get and set a [`VariableOrder`] on a
#' [`CrunchDataset`] or on the [`VariableCatalog`] that the dataset contains.
#'
#' @param x a VariableCatalog or CrunchDataset
#' @param value a valid VariableOrder object
#' @return `ordering` returns a VariableOrder object, while
#' `ordering<-` sets the VariableOrder
#' @name ordering
#' @aliases ordering ordering<-
#' @keywords internal
setGeneric("ordering", function(x) standardGeneric("ordering"))
#' @rdname ordering
setGeneric("ordering<-", function(x, value) standardGeneric("ordering<-"))

#' @rdname ordering
#' @export
setMethod("ordering", "CrunchDataset", function(x) ordering(allVariables(x)))

#' @rdname ordering
#' @export
setMethod("ordering<-", "CrunchDataset", function(x, value) {
    if (is.unforcedVariableCatalog(x@variables)) {
        x@variables <- getDatasetVariables(x)
    }
    ordering(x@variables) <- value
    return(x)
})

#' @rdname ordering
#' @export
setMethod("ordering", "VariableCatalog", function(x) {
    out <- x@order
    out@catalog_url <- self(x)
    return(out)
})

#' @rdname ordering
#' @export
setMethod("ordering<-", "VariableCatalog", function(x, value) {
    stopifnot(inherits(value, "VariableOrder"))

    if (!identical(ordering(x)@graph, value@graph)) {
        ## Give deprecation warning (the first time only per session)
        warn_once(
            "Hey! There's a new way to organize variables within ",
            "datasets: the 'folder' methods. They're easier to use and ",
            "more reliable. See `?mv`, `?cd`, and others for details, and ",
            "`vignettes('variable-order', package='crunch')` for examples. ",
            "You're seeing this message because you're still using the ",
            "ordering<- method, which is fine today, but it will be going ",
            "away in the future, so check out the new methods. ",
            option = "crunch.already.shown.folders.msg"
        )

        ## Validate.
        bad.entities <- setdiff(urls(value), urls(x))
        if (length(bad.entities)) {
            halt(
                pluralize("Variable URL", length(bad.entities)),
                " referenced in Order not present in catalog: ",
                serialPaste(bad.entities)
            )
        }

        order_url <- shojiURL(x, "orders", "hier")
        ## Update on server
        crPUT(order_url, body = toJSON(value))
        ## Drop cache for dataset folders
        dropCache(paste0(datasetReference(x), "folders/"))
        ## Refresh
        x@order <- VariableOrder(crGET(order_url))
    }
    return(x)
})

#' @rdname ordering
#' @export
setMethod("ordering", "DatasetCatalog", function(x) {
    .Deprecated(
        msg = "dataset 'ordering' is deprecated. Use the dataset ",
        "folder API instead. See 'vignette(\"projects\")'."
    )
    out <- DatasetOrder(crGET(shojiURL(x, "orders", "order")))
    out@catalog_url <- self(x)
    return(out)
})

#' @rdname ordering
#' @export
setMethod("ordering", "ProjectFolder", function(x) {
    out <- DatasetCatalog(crGET(shojiURL(x, "catalogs", "datasets")))
    return(ordering(out))
})

.stopDatasetOrderSetter <- function(x, value) {
    halt(
        "Hi there! `ordering<-` no longer works to organize datasets. ",
        " There's a new way to organize datasets within ",
        "projects: the 'folder' methods. They're easier to use and ",
        "more reliable, just like the folder methods for organizing ",
        "variables. See `vignette('projects', package='crunch')` for ",
        "details."
    )
}

#' @rdname ordering
#' @export
setMethod("ordering<-", "DatasetCatalog", .stopDatasetOrderSetter)

#' @rdname ordering
#' @export
setMethod("ordering<-", "ProjectFolder", .stopDatasetOrderSetter)

#' Copy the variable order from one dataset to another.
#'
#' `copyOrder` is deprecated and will be removed in a future version. Instead,
#' you should use the [`copyFolders`] function.
#'
#' @param source the dataset you wan to copy the order from
#' @param target the dataset you want to copy the order to
#' @return returns an object of class [`VariableOrder`] (which can be assigned
#' to a dataset with [`ordering`])
#' @examples
#' \dontrun{
#' ordering(ds) <- copyOrder(ds1, ds)
#' }
#' @export
copyOrder <- function(source, target) {
    if (!is.dataset(source) | !is.dataset(target)) {
        halt("Both source and target must be Crunch datasets.")
    }

    warning(
        "There's a new way to copy ordering and folders: `copyFolders`!",
        "It uses Crunch's new folders system which is easier to use and more ",
        "reliable. `copyOrder` will be removed shortly, so please change your ",
        "code to use `copyFolders`. See `?copyFolders` for more information."
    )

    ord <- entities(ordering(source))

    # make url and alias maps
    url_to_alias_source <- as.list(
        structure(aliases(allVariables(source)), .Names = urls(allVariables(source)))
    )
    alias_to_url_target <- as.list(
        structure(urls(allVariables(target)), .Names = aliases(allVariables(target)))
    )

    new_ord <- lapply(ord, copyOrderGroup,
        source_map = url_to_alias_source,
        target_map = alias_to_url_target
    )

    # drop any null entities, those that were not found in target but in source
    new_ord <- removeMissingEntities(new_ord)
    new_ord <- do.call(VariableOrder, new_ord)

    # set catalog URL so show methods work on the new ordering
    new_ord@catalog_url <- variableCatalogURL(target)

    return(new_ord)
}

#' Copy the order of a `VariableGroup` (or individual variable URL) from `VariableOrder`
#'
#'
#' @param group the group or variable URL to be copied
#' @param source_map url to alias map for source variables
#' @param target_map alias to url map for target variables
#' @return returns either a [`VariableGroup`] (if a group is supplied) or a URL
#' (if just a variable URL is supplied)
#' @keywords internal
copyOrderGroup <- function(group, source_map, target_map) {
    # if there is a single element in group, and it is a character,
    # just return the URL in the target.
    if (length(group) == 1 & is.character(group)) {
        return(target_map[[source_map[[group]]]] %||% NA_character_)
    }

    # there are groups, so recurse
    ents <- lapply(entities(group), copyOrderGroup,
        source_map = source_map, target_map = target_map
    )

    return(VariableGroup(name(group), ents))
}
