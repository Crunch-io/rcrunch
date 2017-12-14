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

