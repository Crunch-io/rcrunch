## Temporary methods to allow a folder-like interface to dataset organization
## via the existing projects/id/datasets/order/ API

mv.project <- function (x, move_this, path) {
    dscat <- datasets(x)

    if (is.dataset(move_this)) {
        ## See if this dataset is already in the project and put it there if not
        if (!(self(move_this) %in% urls(dscat))) {
            owner(move_this) <- self(x)
            x <- refresh(x)
        }
        ## Now just keep the URL
        move_this <- self(move_this)
    } else if (!inherits(move_this, "DatasetGroup")) {
        halt("Must provide a Dataset or Dataset group to mv()")
    }

    ## TODO: need to handle / path to move to top level
    ord <- .ensure.group.path(ordering(dscat), path)
    ## TODO: this [[<- method needs to learn about paths
    entities(ord[[path]]) <- c(entities(ord[[path]]), move_this)
    ## with(temp.option(dont.warn.about.dataset.order)),
    ordering(dscat) <- ord
    return(x)
}

mkdir.project <- function (x, path) {
    ## with(temp.option(dont.warn.about.dataset.order)),
    ordering(x) <- .ensure.group.path(ordering(x), path)
    return(x)
}

.ensure.group.path <- function (ord, path) {
    ## This ensures that "path" exists as a DatasetGroup in ord
    ## It makes no API requests to update the server
    path <- parseFolderPath(path)
    fun <- function (x, path) {
        ## Recursive function for internal use
        if (!(path[1] %in% names(x))) {
            ## TODO: use groupClass, or fuggetaboutit?
            entities(x) <- c(entities(x), DatasetGroup(name=path[1], entities=list()))
        }
        if (length(path) > 1) {
            x[[path[1]]] <- fun(x[[path[1]]], path[-1])
        }
        return(x)
    }
    ord <- fun(ord, path)
    return(ord)
}

rmdir.project <- function (x, path) {
    ordering(x)[[path]] <- NULL
}

## cd.project could return structure(group, path=path, order_url) and then mv/mkdir/setOrder could know what object to update. if we need setOrder

datasetURLFromPath <- function (path) {
    ## Given a /path/to/a/dataset, return that dataset's URL
    path <- parseFolderPath(path)
    ## First, pop off the dataset name as the last segment
    dsname <- tail(path, 1)
    path <- path[-length(path)]
    if (length(path) == 0 || path[1] == "~") {
        ## Default, and ~/, is "personal project"
        dscat <- datasets()
        path <- c()
    } else {
        ## Find the project and see if there is any path left
        dscat <- datasets(projects()[[path[1]]])
        path <- path[-1]
        ## Now, let's walk the "ordering" with any path segments remaining
        ord <- ordering(dscat)
        for (segment in path) {
            ord <- ord[[segment]]
        }
        ## Then, select the subset of dscat corresponding to that group
        dscat <- dscat[urls(ord)]
    }
    return(self(dscat[[dsname]]))
}
