#' Make a CrunchBox
#'
#' CrunchBoxes allow you to publish results to the world.
#'
#' @param dataset CrunchDataset
#' @param filters FilterCatalog, or \code{NULL} for no filters. Default all
#' filters in your catalog, \code{filters(dataset)}.
#' @param ... additional metadata for the box, such as "title", "header", etc.
#' @return The URL to the newly created box.
#' @export
crunchBox <- function (dataset, filters=crunch::filters(dataset), ...) {
    ## Validate inputs
    if (missing(dataset) || !is.dataset(dataset)) {
        halt("'dataset' must be a CrunchDataset, potentially subsetted on variables")
    }
    if (is.null(filters)) {
        ## Make it an empty filter catalog so that it has methods we want below
        filters <- FilterCatalog()
    }
    if (!inherits(filters, "FilterCatalog")) {
        halt("'filters' should be a FilterCatalog or NULL")
    }

    ## Subset on non-hidden variables only
    dataset <- dataset[names(dataset)]

    ## Check that we can compute everything without exploding the server
    nvars <- length(variables(dataset))
    nfilt <- length(filters)
    if (boxTooBig(nvars, nfilt)) {
        halt(nvars, " variable", ifelse(nvars == 1, "", "s"),
            " and ", nfilt, " filter", ifelse(nfilt == 1, "", "s"),
            " results in too many cubes to fit in the box. ",
            "Please try again with fewer of either.")
    }

    ## Construct the payload
    payload <- list(filters=lapply(urls(filters), function (x) list(filter=x)),
        ...)
    ## Add "where" after so that it no-ops if variablesFilter returns NULL (i.e. no filter)
    payload$where <- variablesFilter(dataset)

    ## Send it
    out <- crPOST(shojiURL(dataset, "catalogs", "boxdata"),
        body=toJSON(list(element="shoji:entity", body=payload)))
    return(out)
    ## TODO: add function that maps the URL returned to the embed URL
}

## Make this a function so tests can mock it
.boxlimit <- function () 60000L

boxTooBig <- function (nvars, nfilters) {
    ## Make sure that the number of cubes the box will contain is below a threshold
    nvars * (nvars - 1) * (nfilters + 1) > .boxlimit()
}
