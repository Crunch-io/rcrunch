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
    } else if (is.project(x)) {
        ## This is a ProjectFolder, so filter it to only datasets
        ## TODO: explicit test, though this is called in loadDataset
        return(x[types(x) %in% "dataset"])
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
        ## TODO: update listDatasetGadget to use new dataset folder API
        listDatasetGadget(kind, refresh)
    } else {
        ## TODO: figure out what listDatasets should show
        dscat <- selectDatasetCatalog(kind, project, refresh)
        return(names(dscat))
    }
}

## TODO: kill this function
selectDatasetCatalog <- function(kind = c("active", "all", "archived"),
                                 project = NULL,
                                 refresh = FALSE) {
    Call <- match.call()
    if (is.null(project)) {
        ## Default: we'll get the dataset catalog from the API root
        ## TODO: stop using datasets catalog. Default should be personal?
        ## TODO: where should "shared with me" datasets appear?
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

    if (inherits(dataset, "DatasetTuple")) {
        return(entity(dataset))
    }

    if (refresh) {
        ## TODO: drop relevant caches; see deleteDataset
    }
    if (is.character(dataset)) {
        if (is.datasetURL(dataset)) {
            ## Just load it, no other querying needed
            return(loadDatasetFromURL(dataset))
        }
        ## See if "dataset" is a path or name
        found <- lookupDataset(dataset)
        ## Subset as indicated.
        found <- switch(match.arg(kind),
            active = active(found),
            all = found,
            archived = archived(found)
        )
        if (length(found) == 0) {
            halt(dQuote(dataset), " not found")
        }
        ## This odd selecting behavior handles the multiple matches case
        out <- found[[names(found)[1]]]
        if (!is.dataset(out)) {
            ## There is inconsistency btw DatasetCatalog and ProjectFolder
            out <- entity(out)
        }
        return(out)
    } else if (is.whole(dataset)) {
        warning("TODO write a nice warning that you shouldn't do this anymore")
        dsname <- listDatasets(kind = kind, project = project)[dataset]
        if (is.na(dsname)) {
            halt("subscript out of bounds")
        }
        return(loadDataset(
            dsname,
            kind = kind,
            project = project
        ))
    } else {
        halt("TODO write a nice error that this is a bad input")
    }
}

is.datasetURL <- function (x) {
    # /api/ check is for redacted mocks that prune the scheme://host
    # We don't only use that check because web app URLs don't include /api/
    # Note that this does pass through URLs that are to resources inside a
    # dataset, such as /api/datasets/123/variables/.
    # Call `datasetReference()` on the return to get a dataset URL
    is.character(x) && length(x) == 1L && grepl("^http|^/api/", x)
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
    if (is.dataset(x)) {
        return(delete(x, ...))
    }

    if (is.character(x)) {
        if (is.datasetURL(x) && identical(x, datasetReference(x))) {
            url <- x
        } else {
            # Assume it is a path or name
            found <- lookupDataset(x)
            if (length(found) != 1) {
                halt(x, " identifies ", length(found),
                    " datasets. To delete, please identify the dataset uniquely by URL or path.")
            }
            ## We know there is just one now
            url <- urls(found)
        }
        ## Now, delete it
        if (!askForPermission(paste0("Really delete dataset ", x, "?"))) {
            halt("Must confirm deleting dataset")
        }
        crDELETE(url)
        dropCache(sessionURL("projects"))
        dropOnly(sessionURL("datasets"))
    } else {
        halt("deleteDataset requires either a Dataset, a unique dataset name, or a URL")
    }
}

lookupDataset <- function(x, project = NULL) {
    # x is assumed to be either a dataset name or a path to a dataset by name
    # return is a dataset catalog with dataset tuples matching that name,
    # potentially scoped to a specified project

    # First, see if there is a path, and if so, walk it, possibly relative to
    # `project`
    dspath <- parseFolderPath(x)
    x <- tail(dspath, 1)
    if (length(dspath) > 1) {
        project <- cd(project %||% projects(), dspath[-length(dspath)])
    }
    # If don't have a project, query by name
    if (is.null(project)) {
        return(findDatasetsByName(x))
    } else {
        # Filter the project folder to be only datasets matching this name
        return(datasets(project[names(project) == x]))
    }
}
