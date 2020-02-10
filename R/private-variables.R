setMethod("private", "CrunchDataset", function(x) private(folders(x)))

setMethod("private", "VariableCatalog", function(x) private(folders(x)))

setMethod("private", "VariableFolder", function(x) {
  private <- shojiURL(rootFolder(x), "catalogs", "secure", mustWork = FALSE)
  if (is.null(private)) return(VariableFolder(NULL))
  VariableFolder(crGET(private))
})

#' Make Variables Private or Not Private
#' @param x a Variable or subset of a VariableCatalog to move in/out of the private folder
#' @return (invisibly) the Variable or VariableCatalog with the changes
#' @name privatize
#' @aliases privatize deprivatize privatise deprivatise
#' @seealso [`privatizeVariables`]
NULL

#' @rdname privatize
#' @export
setMethod("privatize", "CrunchVariable", function(x) {
  .moveToFolder(private(rootFolder(x)), x)
  # TODO: should these refresh?
  invisible(x)
})
#' @rdname privatize
#' @export
setMethod("privatize", "VariableCatalog", function(x) {
  .moveToFolder(private(rootFolder(x)), x)
  invisible(x)
})

#' @rdname privatize
#' @export
setMethod("deprivatize", "CrunchVariable", function(x) {
  .moveToFolder(rootFolder(x), x)
  invisible(x)
})
#' @rdname privatize
#' @export
setMethod("deprivatize", "VariableCatalog", function(x) {
  .moveToFolder(rootFolder(x), x)
  invisible(x)
})

#' @rdname privatize
#' @export
setMethod("privatise", "CrunchVariable", function(x) {
  .moveToFolder(private(rootFolder(x)), x)
  # TODO: should these refresh?
  invisible(x)
})
#' @rdname privatize
#' @export
setMethod("privatise", "VariableCatalog", function(x) {
  .moveToFolder(private(rootFolder(x)), x)
  invisible(x)
})

#' @rdname privatize
#' @export
setMethod("deprivatise", "CrunchVariable", function(x) {
  .moveToFolder(rootFolder(x), x)
  invisible(x)
})
#' @rdname privatize
#' @export
setMethod("deprivatise", "VariableCatalog", function(x) {
  .moveToFolder(rootFolder(x), x)
  invisible(x)
})

#' Make variables private (or not) within a dataset
#' @param dataset the Dataset to modify
#' @param x `dataset`, for `privateVariables<-`
#' @param variables names or indices of variables to (de)privatize
#' @param value `variables`, for `privateVariables<-`
#' @return (invisibly) `dataset` with the specified variables (de)privatized
#' @seealso [`privatize`]
#' @export
privatizeVariables <- function(dataset, variables) {
  dataset <- mv(dataset, variables, private(dataset))
  return(invisible(refresh(dataset)))
}

#' @rdname privatizeVariables
#' @export
privatiseVariables <- privatizeVariables

#' @rdname privatizeVariables
#' @export
`privateVariables<-` <- function(x, value) privatizeVariables(x, value)

#' @rdname privatizeVariables
#' @export
deprivatizeVariables <- function(dataset, variables) {
  dataset <- mv(dataset, variables, folders(dataset))
  return(invisible(refresh(dataset)))
}

#' @rdname privatizeVariables
#' @export
deprivatiseVariables <- deprivatizeVariables


#' Show the names of a dataset's private variables
#' @param dataset the Dataset
#' @param key the Variable attribute to return. Default is "alias", following
#' `getOption("crunch.namekey.dataset")`.
#' @return a vector of the names of Variables marked as private.
#' @export
privateVariables <- function(dataset, key = namekey(dataset)) {
  sv <- private(dataset)
  out <- variablesBelowFolder(sv, key)
  if (length(out) == 0) return(c()) # to match old behavior
  sort(out)
}
