setMethod("initialize", "CrunchDataset", function(.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    if (is.null(.Object@variables@self)) {
        # This is only NULL when instantiating a fresh dataset object. If
        # subclassing an existing dataset object, the variable catalog will
        # already be populated (and due to subsetting, may not be identical
        # to a fresh pull from the API)
        #
        # TODO: you could use this check to make lazy the fetching of variables
        .Object@variables <- getDatasetVariables(.Object)
        
        # Hidden variables now require folder transversal, so we do this up front
        # so that we don't have to hit API whenever we want a list of active variables
        hidden_dir <- hidden(.Object)
        hidden_vars <- variablesBelowFolder(hidden_dir, "alias")
        .Object@hiddenVariables <- .Object@variables[aliases(.Object@variables) %in% hidden_vars]
        
        # Secure variables also accessed via folder transversal
        private_dir <- private(.Object)
        private_vars <- variablesBelowFolder(private_dir, "alias")
        .Object@privateVariables <- .Object@variables[aliases(.Object@variables) %in% private_vars]
    }
    if (length(.Object@filter@expression) == 0) {
        # Likewise for preserving filters
        activeFilter(.Object) <- NULL
    }
    return(.Object)
})

getDatasetVariables <- function(x) {
    varcat_url <- variableCatalogURL(x)
    ## Add query params
    return(VariableCatalog(crGET(varcat_url, query = list(relative = "on"))))
}

getNrow <- function(dataset) {
    u <- summaryURL(dataset)
    f <- zcl(activeFilter(dataset))
    q <- crGET(u, query = list(filter = toJSON(f)))
    nrows <- as.integer(round(q$unweighted[["filtered"]]))
    return(nrows)
}

#' Test whether a Crunch object belongs to a class
#' @rdname crunch-is
#' @param x an object
#' @return logical
#' @export
is.dataset <- function(x) inherits(x, "CrunchDataset")

#' @rdname describe-entity
#' @export
setMethod("name", "CrunchDataset", function(x) tuple(x)$name)
#' @rdname describe-entity
#' @export
setMethod("name<-", "CrunchDataset", function(x, value) {
    setEntitySlot(x, "name", validateNewName(value))
})
#' @rdname describe-entity
#' @export
setMethod("description", "CrunchDataset", function(x) tuple(x)$description)
#' @rdname describe-entity
#' @export
setMethod("description<-", "CrunchDataset", function(x, value) {
    setEntitySlot(x, "description", value)
})
#' @rdname describe-entity
#' @export
setMethod(
    "startDate", "CrunchDataset",
    function(x) trimISODate(tuple(x)$start_date)
)
#' @rdname describe-entity
#' @export
setMethod("startDate<-", "CrunchDataset", function(x, value) {
    setEntitySlot(x, "start_date", value)
})
#' @rdname describe-entity
#' @export
setMethod(
    "endDate", "CrunchDataset",
    function(x) trimISODate(tuple(x)$end_date)
)
#' @rdname describe-entity
#' @export
setMethod("endDate<-", "CrunchDataset", function(x, value) {
    setEntitySlot(x, "end_date", value)
})
#' @rdname describe-entity
#' @export
setMethod("id", "CrunchDataset", function(x) tuple(x)$id)

#' @rdname describe-entity
#' @export
setMethod("notes", "CrunchDataset", function(x) x@body$notes)
#' @rdname describe-entity
#' @export
setMethod("notes<-", "CrunchDataset", function(x, value) {
    invisible(setEntitySlot(x, "notes", value))
})

#' Get and set the market size for Crunch datasets
#'
#' Crunch Datasets allow you to set a target population size in order to extrapolate
#' population estimates from survey percentages. These functions let you work with
#' the population size and magnitude.
#'
#' @param x a Crunch Dataset
#' @param value For the setters, the `size` or `magnitude` to be set
#' @param size the target population size, to remove a population set to `NULL`
#' @param magnitude the order of magnitude with which to display the population
#' size. Must be either `3`, `6`, or `9` for thousands, millions, and billions respectively.
#' @return `popSize` and `popMagnitude` return the population size or
#' magnitude. `setPopulation` returns the modified dataset.
#' @name population
#' @aliases popSize popMagnitude setPopulation popSize<- popMagnitude<-
NULL

#' @rdname population
#' @export
setMethod("popSize", "CrunchDataset", function(x) {
    return(settings(x)$population$size)
})

#' @rdname population
#' @export
setMethod("popSize<-", "CrunchDataset", function(x, value) {
    setPopulation(x, size = value)
})

#' @rdname population
#' @export
setMethod("popMagnitude", "CrunchDataset", function(x) {
    return(settings(x)$population$magnitude)
})

#' @rdname population
#' @export
setMethod("popMagnitude<-", "CrunchDataset", function(x, value) {
    setPopulation(x, magnitude = value)
})

#' @rdname population
#' @export
setMethod("setPopulation", "CrunchDataset", function(x, size, magnitude) {
    # Population and magnitude can be an integer, NULL or missing. Moreover if
    # a dataset doesn't have a population both population size and magnitude need
    # to be sent together. The logic for setting magnitude is:
    # If either size or magnitude are missing attempt to set the other value
    # If size is NULL clear population
    # If magnitude is missing and hasn't been set, default to thousands
    # If size is missing and hasn't been set, error
    pop <- settings(x)$population
    if (missing(size)) {
        if (is.null(pop$size)) {
            halt(
                "Dataset does not have a population, please set one before ",
                "attempting to change magnitude"
            )
        }
        size <- pop$size
    } else if (is.null(size)) {
        settings(x)$population <- NULL
        return(invisible(x))
    }

    if (missing(magnitude)) {
        if (is.null(pop$magnitude)) {
            warning("Dataset magnitude not set, defaulting to thousands")
            magnitude <- 3
        } else {
            magnitude <- pop$magnitude
        }
    }

    if (is.null(magnitude)) {
        halt(
            "Magnitude cannot be set to `NULL`. Did you mean to remove ",
            "population size with `popSize(x) <- NULL`?"
        )
    }
    if (!(magnitude %in% c(3, 6, 9))) {
        halt("Magnitude must be either 3, 6, or 9")
    }

    settings(x)$population <- list(magnitude = magnitude, size = size)
    return(invisible(x))
})

#' Get and set the primary key for a Crunch dataset
#'
#' A primary key is a variable in a dataset that has a unique value for every
#' row. A variable must be either numeric or text type and have no duplicate or
#' missing values. A primary key on a dataset causes appends to that dataset
#' that have the rows with the same primary key value(s) as the first dataset
#' to update the existing rows rather than inserting new ones.
#'
#' @param x a Dataset
#' @param value For the setter, a single Variable to use as the primary key or
#' `NULL` to remove the primary key.
#' @return Getter returns the Variable object that is used as the primary key
#' (`NULL` if there is no primary key); setter returns `x` duly modified.
#' @export
pk <- function(x) {
    stopifnot(is.dataset(x))
    pk_var <- ShojiEntity(crGET(shojiURL(x, "fragments", "pk")))$pk
    if (length(pk_var)) {
        return(x[[pk_var[[1]]]])
    } else {
        return(NULL)
    }
}

#' @rdname pk
#' @export
`pk<-` <- function(x, value) {
    stopifnot(is.dataset(x))
    pk_url <- shojiURL(x, "fragments", "pk")
    if (is.null(value)) {
        crDELETE(pk_url)
    } else {
        crPOST(pk_url, body = toJSON(list(pk = I(self(value)))))
    }
    invisible(x)
}

trimISODate <- function(x) {
    ## Drop time from datestring if it's only a date
    if (is.character(x) && nchar(x) > 10 && endsWith(x, "T00:00:00+00:00")) {
        x <- substr(x, 1, 10)
    }
    return(x)
}

as.dataset <- function(x, tuple = DatasetTuple()) {
    out <- CrunchDataset(x)
    tuple(out) <- tuple
    return(out)
}

#' Dataset dimensions
#'
#' @param x a Dataset
#' @return integer vector of length 2, indicating the number of rows and
#' non-hidden variables in the dataset. Array subvariables are excluded from
#' the column count.
#' @seealso [base::dim()]
#' @name dim-dataset
NULL

#' @rdname dim-dataset
#' @export
setMethod("dim", "CrunchDataset", function(x) c(getNrow(x), ncol(x)))

#' @rdname dim-dataset
#' @export
setMethod("ncol", "CrunchDataset", function(x) length(variables(x)))

namekey <- function(x = NULL) {
    if (is.variable(x)) {
        return(match.arg(getOption("crunch.namekey.array"), c("alias", "name")))
    } else if (inherits(x, "VariableOrder") || inherits(x, "VariableGroup")) {
        return(match.arg(getOption("crunch.namekey.variableorder"), c("name", "alias")))
    } else {
        return(match.arg(getOption("crunch.namekey.dataset"), c("alias", "name")))
    }
}

#' @rdname describe-catalog
#' @export
setMethod("names", "CrunchDataset", function(x) {
    getIndexSlot(variables(x), namekey(x))
})

setMethod("tuple", "CrunchDataset", function(x) x@tuple)
setMethod("tuple<-", "CrunchDataset", function(x, value) {
    x@tuple <- value
    return(x)
})

#' @rdname refresh
#' @export
setMethod("refresh", "CrunchDataset", function(x) {
    url <- self(x)
    dropCache(url)
    dropOnly(shojiURL(x, "catalogs", "parent"))
    out <- loadDatasetFromURL(url)
    ## Because dataset may have changed catalogs, check this cache too
    dropOnly(shojiURL(out, "catalogs", "parent"))

    ## So that they test correctly, prune entity body attributes from the tuple
    old_tuple <- tuple(x)@body
    new_tuple <- tuple(out)@body
    tuple(out)@body <- modifyList(
        old_tuple,
        new_tuple[intersect(names(new_tuple), names(old_tuple))]
    )

    ## Make sure the activeFilter's dataset_url is also up to date
    filt <- activeFilter(x)
    if (!is.null(filt)) {
        filt@dataset_url <- self(out)
    }
    activeFilter(out) <- filt
    return(out)
})

#' @export
as.list.CrunchDataset <- function(x, ...) {
    lapply(seq_along(variables(x)), function(i) x[[i]])
}

joins <- function(x) ShojiCatalog(crGET(shojiURL(x, "catalogs", "joins")))

variableCatalogURL <- function(dataset) {
    ## Get the variable catalog URL that corresponds to an object
    if (class(dataset) == "VariableCatalog") {
        return(self(dataset))
    }
    if (!is.dataset(dataset)) {
        dataset <- ShojiObject(crGET(datasetReference(dataset)))
    }
    return(shojiURL(dataset, "catalogs", "variables"))
}

summaryURL <- function(x) shojiURL(x, "views", "summary")

cubeURL <- function(x) {
    if (is.dataset(x)) {
        return(shojiURL(x, "views", "cube"))
    } else {
        ## :( Construct the URL
        return(absoluteURL("./cube/", datasetReference(x)))
    }
}

#' View a Crunch Object in the Web Application
#'
#' Convenience function that will use your system's "open" command to open
#' a Crunch object in our web application in your default browser.
#'
#' @param x a Crunch Dataset or Variable
#' @return Nothing; called for side effect of opening your web browser.
#' @name webApp
#' @importFrom utils browseURL
#' @export
webApp <- function(x) browseURL(APIToWebURL(x))

#' as.environment method for CrunchDataset
#'
#' This method allows you to `eval` within a Dataset.
#'
#' @param x CrunchDataset
#' @return an environment in which named objects are (promises that return)
#' CrunchVariables.
setMethod("as.environment", "CrunchDataset", function(x) {
    out <- new.env()
    out$.crunchDataset <- x
    with(out, {
        ## Note the difference from as.data.frame: not as.vector here
        for (a in aliases(allVariables(x))) {
            eval(substitute(delayedAssign(v, .crunchDataset[[v]]), list(v = a)))
        }
    })
    return(out)
})

#' Get and set the owner of a dataset
#'
#' @param x CrunchDataset
#' @param value For the setter, either a URL (character) or a Crunch object
#' with a `self` method. Users and Projects are valid objects to assign
#' as dataset owners.
#' @return The dataset.
#' @name dataset-owner
#' @aliases owner owner<-
NULL

#' @rdname dataset-owner
#' @export
setMethod("owner", "CrunchDataset", function(x) x@body$owner) ## Or can get from catalog

#' @rdname dataset-owner
#' @export
setMethod("owner<-", "CrunchDataset", function(x, value) {
    if (!is.character(value)) {
        ## Assume we have a User or Project. Get self()
        ## Will error if self isn't defined, and if a different entity type is
        ## given, the PATCH below will 400.
        value <- self(value)
    }
    ## TODO: .moveToFolder(value, x)
    x <- setEntitySlot(x, "owner", value)
    return(x)
})

#' View and modify dataset-level settings
#'
#' These methods allow access and control over dataset settings. Currently
#' supported settings include:
#' * User Authorizations for view-only users ('viewers_can_export', 'viewers_can_share', and
#' 'viewers_can_change_weight'); and
#' * 'weight', which determines the default weighting variable for the dataset
#'  Additional settings will be added in the future. See
#' http://docs.crunch.io/#fragments, under 'Settings', for an up-to-date
#' list of settings supported throughout the Crunch system. Clients may also
#' provide and use custom settings if they choose.
#' @param x CrunchDataset
#' @param value A settings object (`ShojiEntity`), for the setter
#' @return The getter returns a settings object (`ShojiEntity`). The setter
#' returns the dataset (`x`), duly modified.
#' @examples
#' \dontrun{
#' settings(ds)
#' settings(ds)$viewers_can_export <- TRUE
#' settings(ds)$weight <- ds$myWeightVariable
#' }
#' @export
settings <- function(x) {
    stopifnot(is.dataset(x))
    return(ShojiEntity(crGET(shojiURL(x, "fragments", "settings"))))
}

#' @rdname settings
#' @export
"settings<-" <- function(x, value) {
    stopifnot(is.dataset(x))
    updateEntity(settings(x), value)
    return(x)
}

#' View or set a dashboard URL
#'
#' You can designate a dashboard that will show when the dataset is loaded in
#' the Crunch web app. This dashboard could be a Crunch Shiny ("Crunchy") app,
#' a CrunchBox, an RMarkdown website or something else.
#'
#' @param x CrunchDataset
#' @param value For the setter, a URL (character) or `NULL` to unset the
#' dashboard.
#' @return The getter returns a URL (character) or `NULL`. The setter
#' returns the dataset (`x`).
#' @examples
#' \dontrun{
#' dashboard(ds) <- "https://shiny.crunch.io/example/"
#' }
#' @export
dashboard <- function(x) {
    stopifnot(is.dataset(x))
    app_settings <- x@body[["app_settings"]] %||% list()
    whaam <- app_settings[["whaam"]] %||% list()
    return(whaam[["dashboardUrl"]])
}

#' @rdname dashboard
#' @export
setDashboardURL <- function(x, value) {
    setEntitySlot(x, "app_settings", list(whaam = list(dashboardUrl = value)))
}

#' @rdname dashboard
#' @export
"dashboard<-" <- setDashboardURL
