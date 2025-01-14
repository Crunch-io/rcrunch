#' Get and set VariableOrder
#'
#' The `ordering` methods allow you to get a [`VariableOrder`] on a
#' [`CrunchDataset`] or on the [`VariableCatalog`] that the dataset contains.
#'
#' Crunch datasets work with folders, and the ordering is deprecated. It is no
#' longer possible to set the ordering of a variable catalog from rcrunch.
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
        halt(
            "Hey! There's a new way to organize variables within ",
            "datasets: the 'folder' methods. They're easier to use and ",
            "more reliable. See `?mv`, `?cd`, and others for details, and ",
            "`vignettes('variable-order', package='crunch')` for examples. ",
            "The old ordering<- method no longer works."
        )
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

    halt(
        "There's a new way to copy ordering and folders: `copyFolders`!",
        "It uses Crunch's new folders system which is easier to use and more ",
        "reliable. `copyOrder` has been removed, so please change your ",
        "code to use `copyFolders`. See `?copyFolders` for more information."
    )
}
