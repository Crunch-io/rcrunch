init.CrunchDataset <- function (.Object, ...) {
    .Object <- callNextMethod(.Object, ...)
    .Object@variables <- getDatasetVariables(.Object)
    activeFilter(.Object) <- NULL
    return(.Object)
}
setMethod("initialize", "CrunchDataset", init.CrunchDataset)

getDatasetVariables <- function (x) {
    varcat_url <- variableCatalogURL(x)
    ## Add query params
    return(VariableCatalog(crGET(varcat_url, query=list(relative="on"))))
}

getNrow <- function (dataset) {
    u <- summaryURL(dataset)
    f <- zcl(activeFilter(dataset))
    q <- crGET(u, query=list(filter=toJSON(f)))
    nrows <- as.integer(round(q$unweighted[["filtered"]]))
    return(nrows)
}

#' Is it?
#' @rdname crunch-is
#' @param x an object
#' @return logical
#' @export
is.dataset <- function (x) inherits(x, "CrunchDataset")

#' Name, alias, and description for Crunch objects
#'
#' @param x a Dataset or Variable.
#' @param object Same as \code{x} but for the \code{alias} method, in order to
#' match the generic from another package. Note that \code{alias} is only
#' defined for Variables.
#' @param value For the setters, a length-1 character vector to assign
#' @return Getters return the character object in the specified slot; setters
#' return \code{x} duly modified.
#' @name describe
#' @aliases describe name name<- description description<- alias<- startDate startDate<- endDate endDate<- notes notes<-
#' @seealso \code{\link{Categories}} \code{\link{describe-catalog}}
NULL

#' @rdname describe
#' @export
setMethod("name", "CrunchDataset", function (x) tuple(x)$name)
#' @rdname describe
#' @export
setMethod("name<-", "CrunchDataset", function (x, value) {
    invisible(setTupleSlot(x, "name", validateNewName(value)))
})
#' @rdname describe
#' @export
setMethod("description", "CrunchDataset", function (x) tuple(x)$description)
#' @rdname describe
#' @export
setMethod("description<-", "CrunchDataset", function (x, value) {
    setTupleSlot(x, "description", value)
})
#' @rdname describe
#' @export
setMethod("startDate", "CrunchDataset",
    function (x) trimISODate(tuple(x)$start_date))
#' @rdname describe
#' @export
setMethod("startDate<-", "CrunchDataset", function (x, value) {
    setTupleSlot(x, "start_date", value)
})
#' @rdname describe
#' @export
setMethod("endDate", "CrunchDataset",
    function (x) trimISODate(tuple(x)$end_date))
#' @rdname describe
#' @export
setMethod("endDate<-", "CrunchDataset", function (x, value) {
    setTupleSlot(x, "end_date", value)
})
#' @rdname describe
#' @export
setMethod("id", "CrunchDataset", function (x) tuple(x)$id)

#' @rdname describe
#' @export
setMethod("notes", "CrunchDataset", function (x) x@body$notes)
#' @rdname describe
#' @export
setMethod("notes<-", "CrunchDataset", function (x, value) {
    invisible(setEntitySlot(x, "notes", value))
})

trimISODate <- function (x) {
    ## Drop time from datestring if it's only a date
    if (is.character(x) && nchar(x) > 10 && endsWith(x, "T00:00:00+00:00")) {
        x <- substr(x, 1, 10)
    }
    return(x)
}

as.dataset <- function (x, tuple=DatasetTuple()) {
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
#' @seealso \code{\link[base]{dim}}
#' @name dim-dataset
NULL

#' @rdname dim-dataset
#' @export
setMethod("dim", "CrunchDataset",
    function (x) c(getNrow(x), ncol(x)))

#' @rdname dim-dataset
#' @export
setMethod("ncol", "CrunchDataset", function (x) length(variables(x)))

namekey <- function (x=NULL) {
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
setMethod("names", "CrunchDataset", function (x) {
    getIndexSlot(variables(x), namekey(x))
})

setMethod("tuple", "CrunchDataset", function (x) x@tuple)
setMethod("tuple<-", "CrunchDataset", function (x, value) {
    x@tuple <- value
    return(x)
})

#' Get a fresh copy from the server
#'
#' Crunch objects usually keep themselves in sync with the server when you
#' manipulate them, but sometimes they can drift. Maybe someone else has
#' modified the dataset you're working on, or maybe
#' you have modified a variable outside of the context of its dataset.
#' refresh() allows you to get back in sync.
#'
#' @param x pretty much any Crunch object
#' @return a new version of \code{x}
#' @name refresh
#' @aliases refresh
#' @importFrom httpcache dropCache
NULL

#' @rdname refresh
#' @export
setMethod("refresh", "CrunchDataset", function (x) {
    ## Because dataset may have changed catalogs, get the entity url,
    ## check for its parent catalog, get that, and then assemble.
    url <- self(x)
    dropCache(url)
    ent <- crGET(self(x))
    catalog_url <- ent$catalogs$parent %||% tuple(x)@index_url ## %||% for backwards comp.
    dropCache(catalog_url)
    catalog <- DatasetCatalog(crGET(catalog_url))
    out <- as.dataset(ent, tuple=catalog[[url]])

    ## Keep settings in sync
    duplicates(allVariables(out)) <- duplicates(allVariables(x))
    ## Make sure the activeFilter's dataset_url is also up to date
    filt <- activeFilter(x)
    if (!is.null(filt)) {
        filt@dataset_url <- self(out)
    }
    activeFilter(out) <- filt
    return(out)
})

#' Delete a Crunch object from the server
#'
#' These methods delete entities, notably Datasets and Variables within them,
#' from the server. This action is permanent and cannot be undone, so it
#' should not be done lightly. Consider instead using \code{archive}
#' for datasets and \code{\link{hide}} for variables.
#'
#' Deleting requires confirmation. In an interactive session, you will be asked
#' to confirm. To avoid that prompt, or to delete objects from a 
#' non-interactive session, wrap the call in \code{\link{with_consent}} to give
#' your permission to delete.
#'
#' @param x a Crunch object
#' @param ... additional arguments, in the generic
#' @seealso \code{\link{hide}} \code{\link{deleteDataset}}
#' @name delete
#' @aliases delete
NULL

#' @rdname delete
#' @export
setMethod("delete", "CrunchDataset",
    function (x, ...) {
        out <- delete(tuple(x), ...)
        invisible(out)
    })

#' @export
as.list.CrunchDataset <- function (x, ...) {
    lapply(seq_along(variables(x)), function (i) x[[i]])
}

#' See the appended batches of this dataset
#' @param x a \code{CrunchDataset}
#' @return a \code{BatchCatalog}
#' @export
batches <- function (x) BatchCatalog(crGET(shojiURL(x, "catalogs", "batches")))

joins <- function (x) ShojiCatalog(crGET(shojiURL(x, "catalogs", "joins")))

setDatasetVariables <- function (x, value) {
    v <- urls(value)
    x@variables[v] <- value
    ordering(x@variables) <- ordering(value)
    return(x)
}

setMethod("datasetReference", "CrunchDataset", function (x) self(x))

variableCatalogURL <- function (dataset) {
    ## Get the variable catalog URL that corresponds to an object
    if (class(dataset) == "VariableCatalog") return(self(dataset))
    if (!is.dataset(dataset)) {
        dataset <- ShojiObject(crGET(datasetReference(dataset)))
    }
    return(shojiURL(dataset, "catalogs", "variables"))
}

summaryURL <- function (x) shojiURL(x, "views", "summary")

cubeURL <- function (x) {
    if (is.dataset(x)) {
        return(shojiURL(x, "views", "cube"))
    } else {
        ## :( Construct the URL
        return(absoluteURL("./cube/", datasetReference(x)))
    }
}

#' Access a Dataset's Variables Catalog
#'
#' Datasets contain collections of variables. For a few purposes, such as
#' editing variables' metadata, it is helpful to access these variable catalogs
#' more directly.
#'
#' \code{variables} gives just the active variables in the dataset, while
#' \code{allVariables}, as the name suggests, yields all variables, including
#' hidden variables.
#' @param x a Dataset
#' @param value For the setters, a VariableCatalog to assign.
#' @return Getters return VariableCatalog; setters return \code{x} duly
#' modified.
#' @name dataset-variables
#' @aliases dataset-variables variables variables<- allVariables allVariables<-
NULL

#' @rdname dataset-variables
#' @export
setMethod("variables", "CrunchDataset", function (x) active(allVariables(x)))
#' @rdname dataset-variables
#' @export
setMethod("variables<-", c("CrunchDataset", "VariableCatalog"),
    setDatasetVariables)
#' @rdname dataset-variables
#' @export
setMethod("allVariables", "CrunchDataset", function (x) x@variables)
#' @rdname dataset-variables
#' @export
setMethod("allVariables<-", c("CrunchDataset", "VariableCatalog"),
    setDatasetVariables)

setMethod("hidden", "CrunchDataset", function (x) hidden(allVariables(x)))


webURL <- function (x) {
    ## URL to view this dataset in the web app
    stopifnot(is.dataset(x))
    return(paste0(absoluteURL("/", getOption("crunch.api")), "dataset/", id(x)))
}

#' as.environment method for CrunchDataset
#'
#' This method allows you to \code{eval} within a Dataset.
#'
#' @param x CrunchDataset
#' @return an environment in which named objects are (promises that return)
#' CrunchVariables.
setMethod("as.environment", "CrunchDataset", function (x) {
    out <- new.env()
    out$.crunchDataset <- x
    with(out, {
        ## Note the difference from as.data.frame: not as.vector here
        for (a in aliases(allVariables(x))) {
            eval(substitute(delayedAssign(v, .crunchDataset[[v]]), list(v=a)))
        }
    })
    return(out)
})

.releaseDataset <- function (dataset) {
    release_url <- absoluteURL("release/", self(dataset))
    crPOST(release_url, drop=dropCache(self(dataset)))
}

#' Change the owner of a dataset
#'
#' @param x CrunchDataset
#' @param value For the setter, either a URL (character) or a Crunch object
#' with a \code{self} method. Users and Projects are valid objects to assign
#' as dataset owners.
#' @return The dataset.
#' @name dataset-owner
#' @aliases owner owner<-
NULL

#' @rdname dataset-owner
#' @export
setMethod("owner", "CrunchDataset", function (x) x@body$owner) ## Or can get from catalog

#' @rdname dataset-owner
#' @export
setMethod("owner<-", "CrunchDataset", function (x, value) {
    if (!is.character(value)) {
        ## Assume we have a User or Project. Get self()
        ## Will error if self isn't defined, and if a different entity type is
        ## given, the PATCH below will 400.
        value <- self(value)
    }
    x <- setEntitySlot(x, "owner", value)
    return(x)
})


#' Get and set "archived" and "published" status of a dataset
#'
#' "Archived" datasets are excluded from some views. "Draft" datasets are
#' visible only to editors. "Published" is the inverse of "Draft", i.e.
#' \code{is.draft(x)} entails \code{!is.published(x)}. These properties are
#' accessed and set with the "is" methods. The verb functions \code{archive}
#' and \code{publish} are alternate versions of the setters (at least in the
#' \code{TRUE} direction).
#'
#' @param x CrunchDataset
#' @param value logical
#' @return For the getters, the logical value of whether the dataset is
#' archived, in draft mode, or published, where draft and published are
#' inverses. The setters return the dataset.
#' @name archive-and-publish
#' @aliases archive is.archived is.draft is.published is.archived<- is.draft<- is.published<- publish
NULL

#' @rdname archive-and-publish
#' @export
setMethod("is.archived", "CrunchDataset", function (x) tuple(x)$archived)
#' @rdname archive-and-publish
#' @export
setMethod("is.draft", "CrunchDataset", function (x) !is.published(x))
#' @rdname archive-and-publish
#' @export
setMethod("is.published", "CrunchDataset", function (x) tuple(x)$is_published %||% TRUE)

#' @rdname archive-and-publish
#' @export
setMethod("is.archived<-", c("CrunchDataset", "logical"), function (x, value) {
    setTupleSlot(x, "archived", value)
})
#' @rdname archive-and-publish
#' @export
archive <- function (x) {
    is.archived(x) <- TRUE
    return(x)
}
#' @rdname archive-and-publish
#' @export
setMethod("is.draft<-", c("CrunchDataset", "logical"), function (x, value) {
    setTupleSlot(x, "is_published", !value)
})
#' @rdname archive-and-publish
#' @export
setMethod("is.published<-", c("CrunchDataset", "logical"), function (x, value) {
    setTupleSlot(x, "is_published", value)
})
#' @rdname archive-and-publish
#' @export
publish <- function (x) {
    is.published(x) <- TRUE
    return(x)
}

#' View and modify dataset-level settings
#'
#' These methods allow access and control over dataset settings. Currently
#' supported settings include 'viewers_can_export', 'viewers_can_share', and
#' 'viewers_can_change_weight', which govern specific authorizations for users
#' with view-only access to this datset; and 'weight', which is the default
#' weight variable for the dataset, the one that will be set for newly shared
#' users and the one that viewers will have always on if they are not authorized
#' to change weights. Additional settings will be added in the future. See
#' \url{http://docs.crunch.io/#fragments}, under 'Settings', for an up-to-date
#' list of settings supported throughout the Crunch system. Clients may also
#' provide and use custom settings if they choose.
#' @param x CrunchDataset
#' @param value A settings object (\code{ShojiEntity}), for the setter
#' @return The getter returns a settings object (\code{ShojiEntity}). The setter
#' returns the dataset (\code{x}).
#' @examples
#' \dontrun{
#' settings(ds)
#' settings(ds)$viewers_can_export <- TRUE
#' }
#' @export
settings <- function (x) {
    stopifnot(is.dataset(x))
    return(ShojiEntity(crGET(shojiURL(x, "fragments", "settings"))))
}

#' @rdname settings
#' @export
"settings<-" <- function (x, value) {
    stopifnot(is.dataset(x))
    updateEntity(settings(x), value)
    return(x)
}
