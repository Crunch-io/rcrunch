setMethod("secured", "CrunchDataset", function(x) secured(folders(x)))

setMethod("secured", "VariableCatalog", function(x) secured(folders(x)))

setMethod("secured", "VariableFolder", function(x) {
  return(VariableFolder(crGET(shojiURL(rootFolder(x), "catalogs", "secure"))))
})

#' Secure and Unsecure Variables
#' @param x a Variable or subset of a VariableCatalog to secure or move out of the secure folder
#' @return (invisibly) the Variable or VariableCatalog, secured or unsecured
#' @name secure
#' @aliases secure unsecure
#' @seealso [`secureVariables`]
NULL

#' @rdname secure
#' @export
setMethod("secure", "CrunchVariable", function(x) {
  .moveToFolder(secured(rootFolder(x)), x)
  # TODO: should these refresh?
  invisible(x)
})
#' @rdname secure
#' @export
setMethod("secure", "VariableCatalog", function(x) {
  .moveToFolder(secured(rootFolder(x)), x)
  invisible(x)
})

#' @rdname secure
#' @export
setMethod("unsecure", "CrunchVariable", function(x) {
  .moveToFolder(rootFolder(x), x)
  invisible(x)
})
#' @rdname secure
#' @export
setMethod("unsecure", "VariableCatalog", function(x) {
  .moveToFolder(rootFolder(x), x)
  invisible(x)
})

#' Secure and unsecure variables within a dataset
#' @param dataset the Dataset to modify
#' @param x `dataset`, for `secureVariables<-`
#' @param variables names or indices of variables to (un)secure
#' @param value `variables`, for `secureVariables<-`
#' @return (invisibly) `dataset` with the specified variables (un)secured
#' @seealso [`secure`]
#' @export
secureVariables <- function(dataset, variables) {
  dataset <- mv(dataset, variables, secured(dataset))
  return(invisible(refresh(dataset)))
}

#' @rdname secureVariables
#' @export
`securedVariables<-` <- function(x, value) secureVariables(x, value)

#' @rdname secureVariables
#' @export
unsecureVariables <- function(dataset, variables) {
  dataset <- mv(dataset, variables, folders(dataset))
  return(invisible(refresh(dataset)))
}

#' Show the names of a dataset's secure variables
#' @param dataset the Dataset
#' @param key the Variable attribute to return. Default is "alias", following
#' `getOption("crunch.namekey.dataset")`.
#' @return a vector of the names of Variables marked as secured.
#' @export
securedVariables <- function(dataset, key = namekey(dataset)) {
  sv <- secured(dataset)
  out <- variablesBelowFolder(sv, key)
  if (length(out) == 0) return(c()) # to match old behavior
  sort(out)
}
