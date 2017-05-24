#' Get the dataset catalog
#'
#' @param x a \code{ShojiObject}, such as a \code{CrunchProject}. If omitted,
#' the default value for \code{x} means that you will load the user's primary
#' dataset catalog.
#' @param value \code{CrunchDataset} for the setter
#' @return An object of class \code{DatasetCatalog}. The setter returns the
#' project (or other object that contains a dataset catalog with the given
#' dataset added to it (via changing its owner to be
#' the specified object, \code{x}).
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
datasets <- function (x=getAPIRoot()) {
    if (inherits(x, "SearchResults")) {
        ## This is close enough to a dataset catalog
        out <- structure(list(index=x$datasets), class="shoji")
    } else {
        out <- crGET(shojiURL(x, "catalogs", "datasets"))
    }
    DatasetCatalog(out)
}

#' Show the names of all Crunch datasets
#'
#' @param kind character specifying whether to look in active, archived, or all
#' datasets. Default is "active", i.e. non-archived.
#' @param project \code{CrunchProject} entity, character name of a project, or
#' NULL, the default. If a Project entity or reference is supplied, the
#' function will display datasets from that Project's datasets. If NULL,
#' the primary dataset catalog for the user will be used.
#' @param refresh logical: should the function check the Crunch API for new
#' datasets? Default is FALSE.
#' @return Character vector of dataset names, each of which would be a valid
#' input for \code{\link{loadDataset}}
#' @export
listDatasets <- function (kind=c("active", "all", "archived"), project=NULL,
                          refresh=FALSE) {
    dscat <- selectDatasetCatalog(kind, project, refresh)
    return(names(dscat))
}

selectDatasetCatalog <- function (kind=c("active", "all", "archived"),
                                  project=NULL, refresh=FALSE) {

    Call <- match.call()
    if (is.null(project)) {
        ## Default: we'll get the dataset catalog from the API root
        project <- getAPIRoot()
    } else if (!(is.shojiObject(project) || inherits(project, "ShojiTuple"))) {
        ## Project name, URL, or index
        project <- projects()[[project]]
    }
    if (is.null(project)) {
        ## Means a project was specified (like by name) but it didn't exist
        halt("Project ", deparseAndTruncate(eval.parent(Call$project)),
            " is not valid")
    }

    if (refresh) {
        ## drop cache for the ds catalog URL of the "project"
        dropOnly(shojiURL(project, "catalogs", "datasets"))
    }
    ## Ok, get the catalog.
    catalog <- datasets(project)

    ## Subset as indicated
    return(switch(match.arg(kind),
        active=active(catalog),
        all=catalog,
        archived=archived(catalog)))
}

#' Refresh the local list of Crunch datasets
#' @return Nothing. Called for its side effects of setting local environment
#' variables.
#' @export
#' @importFrom httpcache dropOnly
updateDatasetList <- function () {
    warning("updateDatasetList is being deprecated.")
    dropOnly(sessionURL("datasets"))
}

#' Load a Crunch Dataset
#' @param dataset character, the name of a Crunch dataset you have access
#' to. Or, a \code{DatasetTuple}.
#' @param kind character specifying whether to look in active, archived, or all
#' datasets. Default is "active", i.e. non-archived.
#' @param project \code{CrunchProject} entity, character name of a project, or
#' NULL, the default. If a Project entity or reference is supplied, the
#' function will display datasets from that Project's datasets. If NULL,
#' the primary dataset catalog for the user will be used.
#' @param refresh logical: should the function check the Crunch API for new
#' datasets? Default is FALSE.
#' @return An object of class \code{CrunchDataset}
#' @export
loadDataset <- function (dataset, kind=c("active", "all", "archived"), project=NULL, refresh=FALSE) {
    if (!inherits(dataset, "DatasetTuple")) {
        dscat <- selectDatasetCatalog(kind, project, refresh)
        dsname <- dataset
        dataset <- dscat[[dataset]]
        if (is.null(dataset)) {
            ## Check to see if this is a URL, in which case, GET it
            if (startsWith(dsname, "http")) {
                dataset <- CrunchDataset(crGET(dsname))
                tuple(dataset) <- DatasetTuple(entity_url=self(dataset),
                    body=dataset@body,
                    index_url=shojiURL(dataset, "catalogs", "parent"))
                return(dataset)
            }
            halt(dQuote(dsname), " not found")
        }
    }
    return(entity(dataset))
}

#' Delete a dataset from the dataset list
#'
#' This function lets you delete a dataset without first loading it. If you
#' have a dataset that somehow is corrupted and won't load, you can delete it
#' this way.
#'
#' The function also works on CrunchDataset objects, just like
#' \code{\link{delete}}, which may be useful if you have loaded another
#' package that masks the \code{delete} method.
#' @param x The name (character) of a dataset, its (numeric) position in the
#' return of \code{\link{listDatasets}}, or an object of class
#' \code{CrunchDataset}. x can only be of length 1--this function is not
#' vectorized (for your protection).
#' @param ... additional parameters passed to \code{delete}
#' @return (Invisibly) the API response from deleting the dataset
#' @seealso \code{\link{delete}}
#' @export
deleteDataset <- function (x, ...) {
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
