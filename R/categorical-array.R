#' Make a Categorical Array or Multiple Response variable
#'
#' @param subvariables a list of Variable objects to bind together, or a
#' Dataset object containing only the Variables to bind (as in from subsetting
#' a Dataset), or values (e.g. names) of variables corresponding to \code{key}.
#' If omitted, must supply \code{dataset} and \code{pattern}. If specifying
#' values, must include \code{dataset}.
#' @param dataset Argument no longer supported
#' @param name character, the name that the new Categorical Array variable
#' should have. Required.
#' @param selections character, for \code{makeMR}, the names of the
#' categories to mark as the dichotomous selections. Required for
#' \code{makeMR}; ignored in \code{makeArray}.
#' @param ... Optional additional attributes to set on the new variable.
#' @return A VariableDefinition that when added to a Dataset will create the
#' categorical-array or multiple-response variable.
#' @export
makeArray <- function (subvariables, dataset=NULL, name, ...) {

    if (missing(name)) {
        halt("Must provide the name for the new variable")
    }
    if (!is.null(dataset)) {
        warning(dQuote("dataset"), " argument to makeArray is no longer supported",
            call.=FALSE)
    }

    ## Get subvariable URLs
    if (is.dataset(subvariables)) {
        ## as in, if the list of variables is a [ extraction from a Dataset
        subvariables <- allVariables(subvariables)
    }
    if (inherits(subvariables, "VariableCatalog")) {
        subvariables <- urls(subvariables)
    } else if (is.list(subvariables) &&
               all(vapply(subvariables, is.variable, logical(1)))) {
        subvariables <- vapply(subvariables, self, character(1))
    } else {
        halt(dQuote("subvariables"), " cannot be of class ", class(subvariables))
    }

    if (!length(subvariables)) {
        halt("No variables supplied")
    }

    out <- VariableDefinition(subvariables=I(subvariables), name=name,
        type="categorical_array", ...)
    return(out)
}
