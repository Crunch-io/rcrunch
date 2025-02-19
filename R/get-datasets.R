#' Get a catalog of datasets
#'
#' Crunch datasets are collected in folders called "projects". `datasets()` can
#' be used to filter a project's contents to see only datasets (and not other
#' projects). You can also use it to pull a catalog of datasets from search
#' results.
#'
#' The `datasets()<-` assignment function provides an alternative method for
#' moving a dataset into a project. This may be more convenient in some cases
#' than using [mv()].
#' @param x a `ProjectFolder` or `SearchResults` that may contain datasets
#' @param value For assignment, a `CrunchDataset` to move
#' @return When `x` is a `ProjectFolder`, `datasets()` returns the folder with
#' its "index" filtered to contain only datasets; otherwise, it returns an
#' object of class `DatasetCatalog`. The assignment function returns the
#' project `x` with the given dataset added to it.
#' @name datasets
#' @export
#' @examples
#' \dontrun{
#' # Get the names of the datasets contained in a project
#' projects() %>%
#'     cd("Important Clients") %>%
#'     datasets() %>%
#'     names()
#' # The assignment method lets you move a dataset to a project
#' proj <- cd(projects(), "Important Clients")
#' ds <- loadDataset("New important client survey", project = "Studies")
#' datasets(proj) <- ds
#' }
datasets <- function(x = getAPIRoot()) {
    # TODO: deprecate the dataset catalog, i.e. `datasets()`
    if (inherits(x, "SearchResults")) {
        ## This is close enough to a dataset catalog
        out <- structure(list(index = x$datasets), class = "shoji")
    } else if (is.project(x)) {
        ## This is a ProjectFolder, so filter it to only datasets
        ## TODO: explicit test, though this is called in loadDataset
        return(x[types(x) %in% "dataset"])
    } else {
        out <- crGET(paste0(shojiURL(x, "catalogs", "datasets"), "all"))
    }
    DatasetCatalog(out)
}

#' Get the names of datasets in a project
#'
#' `listDatasets()` is a convenience function for quickly seeing what datasets
#' are in a project. It is equivalent to `names(datasets(proj))`, with some
#' additional optional arguments.
#'
#' @param kind character specifying whether to look in active, archived, or all
#' datasets. Default is "active", i.e. non-archived.
#' @param project `ProjectFolder` entity, character name of a project, or
#' `NULL`, the default. If a Project entity or reference is supplied, the
#' function will display datasets from that Project's datasets. If `NULL`,
#' your personal folder will be used.
#' @param refresh logical: should the function check the Crunch API for new
#' datasets? Default is FALSE.
#' @param shiny deprecated, no longer works
#' @return A character vector of dataset names, each of which would be a valid
#' input for [loadDataset()]
#' @export
listDatasets <- function(kind = c("active", "all", "archived"),
                         project = NULL,
                         refresh = FALSE,
                         shiny = FALSE) {
    if (shiny) {
        halt("listDatsets(shiny = TRUE) is no longer supported")
    } else {
        Call <- match.call()
        if (refresh) {
            dropDatasetsCache()
        }
        if (is.null(project)) {
            warn_once(
                "As of crunch 1.26.0, listDatasets() with no project specified ",
                "only lists your 'personal' datasets (those that you created) ",
                "and not those that were shared with you.",
                option = "crunch.list.personal.msg"
            )
            project <- "~"
        }
        if (is.character(project)) {
            project <- cd(projects(), project)
        }
        if (!is.project(project)) {
            halt(
                "Project ", deparseAndFlatten(eval.parent(Call$project)),
                " is not valid"
            )
        }
        ## Grab just the datasets
        dscat <- datasets(project)
        ## Subset as indicated
        kind <- match.arg(kind)
        if (kind == "active") {
            dscat <- active(dscat)
        } else if (kind == "archived") {
            dscat <- archived(dscat)
        }
        return(names(dscat))
    }
}

#' Load a Crunch Dataset
#'
#' This function gives you a Dataset object, which refers to a dataset hosted on
#' the Crunch platform. With this Dataset, you can perform lots of data cleaning
#' and analysis as if the dataset were fully resident on your computer, without
#' having to pull data locally.
#'
#' You can specify a dataset to load by its human-friendly "name", within
#' the project (folder) to find it in. This makes code more
#' readable, but it does mean that if the dataset is renamed or moved to a
#' different folder, your code may no longer work. The fastest, most reliable
#' way to use `loadDataset()` is to provide a URL to the dataset--the dataset's
#' URL will never change.
#'
#' @param dataset character, the path to a Crunch dataset to load, or a
#' dataset URL. If `dataset` is a path to a dataset in a project, the path will
#' be be parsed and walked, relative to `project`, and the  function will look
#' for the dataset inside that project. If `dataset` is just a string and `project`
#' is set to `NULL`, the function will assume that `dataset` is the dataset id.
#' @param kind character specifying whether to look in active, archived, or all
#' datasets. Default is "active", i.e. non-archived.
#' @param project `ProjectFolder` entity, character name (path) to a project.
#' Defaults to the project set in `envOrOption('crunch.default.project')`
#' or "./" (the project root), if the default is not set.
#' @param refresh logical: should the function check the Crunch API for new
#' datasets? Default is `FALSE`.
#' @return An object of class `CrunchDataset`.
#'
#' @examples
#' \dontrun{
#' ds <- loadDatasets("A special dataset", project = "Studies")
#' ds2 <- loadDatasets("~/My dataset", project = "Studies")
#' ds3 <- loadDataset("My dataset", project = projects()[["Studies"]]) # Same as ds2
#' ds4 <- loadDataset("https://app.crunch.io/api/datasets/bd3ad2/")
#' }
#' @export
#' @seealso See [cd()] for details of parsing and walking dataset folder/project
#' paths.
loadDataset <- function(dataset,
                        kind = c("active", "all", "archived"),
                        project = defaultCrunchProject("."),
                        refresh = FALSE) {
    if (inherits(dataset, "DatasetTuple")) {
        return(entity(dataset))
    }

    if (refresh) {
        dropDatasetsCache()
    }
    if (is.character(dataset)) {
        if (is.crunchURL(dataset)) {
            ## Just load it, no other querying needed
            return(loadDatasetFromURL(dataset))
        }
        ## See if "dataset" is a path or name
        found <- lookupDataset(dataset, project = project)
        ## Subset as indicated.
        found <- switch(match.arg(kind),
            active = active(found),
            all = found,
            archived = archived(found)
        )
        if (length(found) == 0) {
            if (missing(project)) {
                warn_once(
                    "Finding datasets by name without specifying a path is no longer supported.",
                    option = "find.dataset.no.project"
                )
            }
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
        warning(
            "'dataset' should be a character dataset name, path, or URL. ",
            "Loading by numeric index is deprecated."
        )
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
        halt(
            "'dataset' should be a character dataset name, path, or URL, ",
            "not an object of class ", class(dataset)
        )
    }
}

is.crunchURL <- function(x) {
    # /api/ check is for redacted mocks that prune the scheme://host
    # We don't only use that check because web app URLs don't include /api/
    # Note that this does pass through URLs that are to resources inside a
    # dataset, such as /api/datasets/123/variables/.
    # Call `datasetReference()` on the return to get a dataset URL
    is.character(x) && length(x) == 1L && grepl("^http|^/api/", x) # nolint
}

loadDatasetFromURL <- function(url) {
    ## Load dataset without touching a dataset catalog
    if (!grepl("/api/", url)) { # nolint
        ## It's a web app URL, probably. Turn it into an API URL
        url <- datasetReference(url)
    }
    dataset <- CrunchDataset(crGET(url))
    tuple(dataset) <- DatasetTuple(
        entity_url = self(dataset),
        body = dataset@body,
        index_url = shojiURL(dataset, "catalogs", "parent")
    )
    return(dataset)
}

lookupDataset <- function(x, project = NULL) {
    # x is assumed to be either a dataset name or a path to a dataset by name
    # return is a dataset catalog with dataset tuples matching that name,
    # potentially scoped to a specified project
    Call <- match.call()

    # First, see if there is a path, and if so, walk it, possibly relative to
    # `project`
    dspath <- parseFolderPath(x)
    x <- tail(dspath, 1)

    if (length(dspath) == 1 && is.null(project)) {
        # This code path used to use the datasets by_name endpoint. However
        # As of 2024-11, that endpoint is no longer very useful because it only
        # surfaces datasets that are in personal folders (going away very soon) &
        # direct dataset shares (deprecated).
        # So we use this to load by dataset id, a nice convenience feature.
        # To get here, a user had to explicitly set `project=NULL` so they're
        # presumably not here accidentally
        pseudo_shoji <- tryCatch({
            ds_base_url <- absoluteURL("datasets/", envOrOption("crunch.api"))
            ds_entity <- crGET(paste0(ds_base_url, "/", x))
            # Need to make this pseudo DatasetCatalog to match old API call (because `loadDataset`
            # will later pass it through `active()`/`archived()`)
            structure(list(
                self = ds_base_url,
                index = setNames(list(ds_entity$body), ds_entity$self)
            ), class = "shoji")
        }, error = function(...) NULL) # But if we don't find it just return empty catalog

        return(DatasetCatalog(pseudo_shoji))
    }

    # Resolve `project`
    if (is.null(project)) {
        project <- projects()
    } else if (!is.project(project)) {
        project <- ProjectFolder(crGET(resolveProjectURL(project)))
    }

    if (length(dspath) > 1) {
        project <- cd(project, dspath[-length(dspath)])
    }

    # Filter the project folder to be only datasets matching this name
    return(datasets(project[names(project) == x]))
}
