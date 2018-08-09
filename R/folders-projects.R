## Temporary methods to allow a folder-like interface to dataset organization
## via the existing projects/id/datasets/order/ API

mv.project <- function(x, move_this, path) {
    dscat <- datasets(x)
    ord <- ordering(dscat)

    if (is.dataset(move_this)) {
        ## See if this dataset is already in the project and put it there if not
        if (!(self(move_this) %in% urls(dscat))) {
            owner(move_this) <- self(x)
            x <- refresh(x)
            dscat <- datasets(x)
            ord <- ordering(dscat)
        }
        ## Now just keep the URL
        move_this <- self(move_this)
    } else if (is.character(move_this)) {
        move_these_names <- move_this
        move_this <- entities(ord[move_this])
        ## Now remove that/those from ord
        for (group in move_these_names) {
            ord[[group]] <- NULL
        }
    } else if (!inherits(move_this, "DatasetGroup")) {
        halt("Must provide a Dataset or Dataset group to mv()")
    }

    ord <- .ensure.group.path(ord, path)
    entities(ord[[path]]) <- c(setdiff_entities(entities(ord[[path]]), move_this), move_this)
    with(
        temp.option(crunch.already.shown.ds.order.msg = TRUE),
        ordering(dscat) <- ord
    )
    return(x)
}

mkdir.project <- function(x, path) {
    with(
        temp.option(crunch.already.shown.ds.order.msg = TRUE),
        ordering(x) <- .ensure.group.path(ordering(x), path)
    )
    return(x)
}

.ensure.group.path <- function(ord, path) {
    ## This ensures that "path" exists as a DatasetGroup in ord
    ## It makes no API requests to update the server
    path <- parseFolderPath(path)
    fun <- function(x, path) {
        ## Recursive function for internal use
        if (!(path[1] %in% names(x))) {
            x[[path[1]]] <- list()
        }
        if (length(path) > 1) {
            x[[path[1]]] <- fun(x[[path[1]]], path[-1])
        }
        return(x)
    }
    if (nchar(path[1]) == 0) {
        ## Means the path starts with "/", so we're going to start at the top
        ## level. And since this is a ShojiOrder, we're already at the top level
        ## so just pop the segment off
        path <- path[-1]
    }
    if (length(path)) {
        ord <- fun(ord, path)
    }
    return(ord)
    # .setNestedGroupByName(ord, i=path, value=list())
}

rmdir.project <- function(x, path) {
    to_rm <- ordering(x)[[path]]
    if (is.null(to_rm)) {
        ## Great, it already doesn't exist.
        return(x)
    } else if (length(entities(to_rm))) {
        halt(
            "Cannot remove '", path, "' because it is not empty. ",
            "Move its contents somewhere else and then retry."
        )
    }
    with(
        temp.option(crunch.already.shown.ds.order.msg = TRUE),
        ordering(x)[[path]] <- NULL
    )
}

## cd.project could return structure(group, path=path, order_url) and then mv/mkdir/setOrder could know what object to update. if we need setOrder

datasetURLFromPath <- function(path) {
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
