#' Get information about users who have access to a dataset
#'
#' Get user metadata about all of the users that have access to a particular
#' Crunch object like a dataset or project. Returns a `UserCatalog` object which
#' can be translated into a data.frame with [catalogToDataFrame()] if information
#' needs to be extracted, queried, transformed, etc.
#'
#' @param x a `CrunchDataset`, `DatasetTuple`, or `CrunchProject` object to get
#' users from
#'
#' @return a `UserCatalog` with information about users who have access to the
#' dataset
#'
#' @name users
NULL

.getUsers <- function(x) UserCatalog(crGET(shojiURL(x, "catalogs", "users")))

#' @rdname users
#' @export
setMethod("users", "CrunchDataset", .getUsers)

#' @rdname users
#' @export
setMethod("users", "DatasetTuple", .getUsers)

#' @rdname users
#' @export
setMethod("users", "CrunchProject", .getUsers)

#' Get information about when users last accessed a dataset
#'
#' Gets dataset-user interaction metadata about all of the users that have
#' access to a particular dataset or all of the datasets in a project. Returns
#' a data.frame to easily manipulate, subset, and transform details about users.
#'
#' @param x a dataset object or a project object (e.g. from [projects()])
#' @param user_fields metadata fields about users to include in the dataframe
#' (default: `c("name", "email", "access_time")`)
#'
#' @return a dataframe with information about all users and when they last
#' accessed a particular dataset or all of the datasets in a project
#' @export
lastAccessed <- function(x, user_fields = c("name", "email", "access_time")) {
    if (inherits(x, "CrunchProject")) {
        sets <- selectDatasetCatalog(project = name(x))
        list_of_dfs <- lapply(sets, lastAccessed, user_fields)
        all_users <- do.call(rbind, list_of_dfs)
    } else {
        users_catalog <- users(x)
        all_users <- catalogToDataFrame(users_catalog, keys = user_fields)
        all_users$dataset <- name(x)
        all_users$dataset_id <- id(x)
    }

    return(all_users)
}

