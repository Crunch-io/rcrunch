#' Access a catalog of variables
#'
#' Datasets contain collections of variables. For some purposes, such as
#' editing variables' metadata, it is helpful to access these variable catalogs
#' more directly. Other objects, such as cubes and folders, also define
#' `variables()` methods that expose variable metadata.
#'
#' For datasets, `variables()` returns only the active variables in the dataset,
#' while `allVariables()` returns all variables, including hidden variables.
#' `allVariables()` is not defined for other objects.
#'
#' @param x a Dataset
#' @param value For the setters, a VariableCatalog to assign.
#' @return All methods return a `VariableCatalog` except the `VariableFolder`
#' method, which returns a subset of `x` containing only variable references.
#' Assignment functions return `x` with the changes made.
#' @name variables
#' @aliases dataset-variables variables variables<- allVariables allVariables<-
#' @export
setGeneric("variables", function(x) standardGeneric("variables"))
#' @rdname variables
setGeneric("variables<-", function(x, value) standardGeneric("variables<-"))
#' @rdname variables
setGeneric("allVariables", function(x) standardGeneric("allVariables"))
#' @rdname variables
setGeneric(
    "allVariables<-",
    function(x, value) standardGeneric("allVariables<-")
)

#' @rdname variables
#' @export
setMethod("variables", "CubeDims", function(x) {
    ind <- lapply(x@.Data, vget("references"))
    return(.pseudoCatalog(ind))
})

#' @rdname variables
#' @export
setMethod("variables", "CrunchCube", function(x) {
    out <- variables(dimensions(x))
    out@index <- c(out@index, measures(x)@index)
    return(out)
})

setDatasetVariables <- function(x, value) {
    v <- urls(value)
    x@variables[v] <- value
    ordering(x@variables) <- ordering(value)
    return(x)
}

#' @rdname variables
#' @export
setMethod("variables", "CrunchDataset", function(x) {
    variables <- allVariables(x)
    variables <- variables[!(aliases(variables) %in% hiddenVariables(x, "alias"))]
    variables <- variables[!(aliases(variables) %in% privateVariables(x, "alias"))]
    variables
})
#' @rdname variables
#' @export
setMethod(
    "variables<-", c("CrunchDataset", "VariableCatalog"),
    setDatasetVariables
)
#' @rdname variables
#' @export
setMethod("allVariables", "CrunchDataset", function(x) x@variables)
#' @rdname variables
#' @export
setMethod(
    "allVariables<-", c("CrunchDataset", "VariableCatalog"),
    setDatasetVariables
)

#' @rdname variables
#' @export
setMethod("variables", "SearchResults", function(x) {
    ## Close enough to a catalog object
    VariableCatalog(structure(list(index = x$variables), class = "shoji"))
})

#' @rdname variables
#' @export
setMethod("variables", "VariableFolder", function(x) {
    x[!(types(x) %in% "folder")]
})
