#' Get information about when users last accessed a dataset
#'
#' Useful to get dataset-user interaction metadata about all of the users that
#' have access to a particular dataset.
#'
#' @param dataset_entity a dataset object or a dataset entity (e.g. a single
#' entry from `selectDatasetCatalog()`)
#' @param user_fields metadata fields about users to include in the dataframe
#' (default: c("name", "email", "access_time"))
#'
#' @return a dataframe with information about all users for a particular dataset
#' @export
userInfo <- function (dataset_entity, user_fields = c("name", "email", "access_time")) {
    users_catalog <- ShojiCatalog(crGET(paste0(self(dataset_entity),"users/")))
    all_users <- catalogToDataFrame(users_catalog, keys = user_fields)
    all_users$dataset <- name(dataset_entity)
    all_users$dataset_id <- id(dataset_entity)

    return(all_users)
}

#' Get information about when users last any of the datasets in a project
#'
#' Useful to get dataset-user interaction metadata about all of the users that have
#' access to all of the datasets in a project.
#'
#' @param project a character of the project name to gather metadata from
#' @param ... additional arguments passed to [userInfo()] (e.g. user_fields)
#'
#' @return a dataframe with information about all users for a particular dataset
#' @export
projectUserInfo <- function (project, ...) {
    sets <- selectDatasetCatalog(project = project)

    list_of_dfs <- base::lapply(sets, userInfo, ...)

    return(do.call(rbind, list_of_dfs))
}

