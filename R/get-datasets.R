#' Get the dataset catalog
#'
#' Crunch datasets are associated with catalogs. A project catalog will
#' have a set of datasets associated with it, as will a user or team. This
#' function allows you to get or modify the datasets associated with a catalog.
#' @param x a `ShojiObject`, such as a `ProjectFolder`. If omitted,
#' the function will load the user's primary dataset catalog. #'
#' @param value `CrunchDataset` for the setter
#' @return An object of class `DatasetCatalog`. The setter returns the
#' project (or other object that contains a dataset catalog with the given
#' dataset added to it (via changing its owner to be
#' the specified object, `x`).
#' @name datasets
#' @export
#' @examples
#' \dontrun{
#' # Get the primary dataset catalog
#' mydatasets <- datasets()
#' # Can load a dataset from that
#' ds <- loadDataset(mydatasets[["Dataset name"]])
#' # Can use the same function to get the dataset catalog for a project
#' proj <- projects()[["Project name"]]
#' projdatasets <- datasets(proj)
#' # The assignment method lets you move a dataset to a project
#' datasets(proj) <- ds
#' }
datasets <- function(x = getAPIRoot()) {
    if (inherits(x, "SearchResults")) {
        ## This is close enough to a dataset catalog
        out <- structure(list(index = x$datasets), class = "shoji")
    } else {
        out <- crGET(shojiURL(x, "catalogs", "datasets"))
    }
    DatasetCatalog(out)
}

#' Show the names of all Crunch datasets associated with a catalog
#'
#' If `shiny` is TRUE the function launches a shiny gadget which allows you to
#' navigate your Crunch projects and datasets. This is useful if you can't
#' remember a dataset's project and also saves typing long dataset names.
#'
#' @param kind character specifying whether to look in active, archived, or all
#' datasets. Default is "active", i.e. non-archived.
#' @param project `ProjectFolder` entity, character name of a project, or
#' NULL, the default. If a Project entity or reference is supplied, the
#' function will display datasets from that Project's datasets. If NULL,
#' the primary dataset catalog for the user will be used.
#' @param refresh logical: should the function check the Crunch API for new
#' datasets? Default is FALSE.
#' @param shiny logical: launch a shiny gadget to help select the right dataset. The
#' gadget will return a valid `loadDataset()` call which loads the selected dataset.
#' @return Character vector of dataset names, each of which would be a valid
#' input for [loadDataset()]
#' @export
listDatasets <- function(kind = c("active", "all", "archived"),
                         project = NULL,
                         refresh = FALSE,
                         shiny = FALSE) {
    if (shiny) {
        listDatasetGadget(kind, refresh)
    } else {
        dscat <- selectDatasetCatalog(kind, project, refresh)
        return(names(dscat))
    }
}

selectDatasetCatalog <- function(kind = c("active", "all", "archived"),
                                 project = NULL,
                                 refresh = FALSE) {
    Call <- match.call()
    if (is.null(project)) {
        ## Default: we'll get the dataset catalog from the API root
        project <- datasets()
    } else if (!(is.shojiObject(project) || inherits(project, "ShojiTuple"))) {
        ## Project name, URL, or index
        project <- projects()[[project]]
    }
    if (is.null(project)) {
        ## Means a project was specified (like by name) but it didn't exist
        halt(
            "Project ", deparseAndFlatten(eval.parent(Call$project)),
            " is not valid"
        )
    }

    if (refresh) {
        project <- refresh(project)
    }

    if (is.project(project)) {
        # Keep only datasets if ProjectFolder
        project <- project[types(project) %in% "dataset"]
    }
    ## Subset as indicated
    return(switch(match.arg(kind),
        active = active(project),
        all = project,
        archived = archived(project)
    ))
}

#' Load a Crunch Dataset
#'
#' @param dataset character, the name of a Crunch dataset that you have access
#' to, or a `DatasetTuple`.
#' @param kind character specifying whether to look in active, archived, or all
#' datasets. Default is "active", i.e. non-archived.
#' @param project `ProjectFolder` entity, character name of a project, or
#' `NULL`, the default. If a Project entity or reference is supplied, the
#' function will display datasets from that Project's datasets. If `NULL`,
#' the primary dataset catalog for the user will be used.
#' @param refresh logical: should the function check the Crunch API for new
#' datasets? Default is `FALSE`.
#' @return An object of class `CrunchDataset`
#'
#' @examples
#' \dontrun{
#' dsName <- listDatasets()[1]
#' ds <- loadDatasets(dsName)
#' }
#' @export
loadDataset <- function(dataset,
                        kind = c("active", "all", "archived"),
                        project = NULL,
                        refresh = FALSE) {

    if (is.character(dataset) && grepl("^http|^/api/", dataset)) {
        # either /api/ is needed for detecting URLs from redacted mocks.
        # TDOO: a more robust detection method that works with other redactions?
        return(loadDatasetFromURL(dataset))
    }

    if (inherits(dataset, "DatasetTuple")) {
        return(entity(dataset))
    }

    if (is.character(dataset)) {
        ## See if "dataset" is a path
        dspath <- parseFolderPath(dataset)
        dataset <- tail(dspath, 1)
        if (length(dspath) > 1) {
            project <- dspath[-length(dspath)]
        }
    }
    dscat <- selectDatasetCatalog(kind, project, refresh)
    dsname <- dataset
    dataset <- dscat[[dataset]]
    if (is.null(dataset)) {
        halt(dQuote(dsname), " not found")
    } else if (inherits(dataset, "DatasetTuple")) {
        dataset <- entity(dataset)
    }
    return(dataset)
}

loadDatasetFromURL <- function(url) {
    ## Load dataset without touching a dataset catalog
    if (!grepl("/api/", url)) {
        ## It's a web app URL, probably. Turn it into an API URL
        url <- webToAPIURL(url)
    }
    dataset <- CrunchDataset(crGET(url))
    tuple(dataset) <- DatasetTuple(
        entity_url = self(dataset),
        body = dataset@body,
        index_url = shojiURL(dataset, "catalogs", "parent")
    )
    return(dataset)
}

#' Delete a dataset from the dataset list
#'
#' This function lets you delete a dataset without first loading it. If you
#' have a dataset that somehow is corrupted and won't load, you can delete it
#' this way.
#'
#' The function also works on CrunchDataset objects, just like
#' [delete()], which may be useful if you have loaded another
#' package that masks the [delete()] method.
#' @param x The name (character) of a dataset, its (numeric) position in the
#' return of [listDatasets()], or an object of class
#' `CrunchDataset`. x can only be of length 1--this function is not
#' vectorized (for your protection).
#' @param ... additional parameters passed to `delete`
#' @return (Invisibly) the API response from deleting the dataset
#' @seealso [delete()]
#' @export
deleteDataset <- function(x, ...) {
    if (!is.dataset(x)) {
        if (is.numeric(x)) {
            x <- listDatasets()[x]
            if (is.na(x)) {
                halt("subscript out of bounds")
            }
        }
        x <- datasets()[[x]]
    }
    out <- delete(x, ...)
    invisible(out)
}
