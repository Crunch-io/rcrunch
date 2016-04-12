##' Make a Categorical Array or Multiple Response variable
##'
##' @param subvariables a list of Variable objects to bind together, or a
##' Dataset object containing only the Variables to bind (as in from subsetting
##' a Dataset), or values (e.g. names) of variables corresponding to \code{key}.
##' If omitted, must supply \code{dataset} and \code{pattern}. If specifying
##' values, must include \code{dataset}.
##' @param dataset the Crunch Dataset to which the variables in
##' \code{subvariables} belong, or in which to search for variables based
##' on \code{pattern}. If omitted, \code{subvariables} must exist and all
##' Variables in the list must belong to the same Dataset
##' @param pattern An optional regular expression to search for variables to
##' bind within \code{dataset}.
##' Note that this argument is deprecated. If you wish to grep, you can
##' \code{grep(pattern, aliases(variables(dataset)))} or similar outside this
##' function.
##' @param key character, the name of the Variable field in which to search
##' with \code{pattern}. Default is 'alias'.
##' @param name character, the name that the new Categorical Array variable
##' should have. Required.
##' @param selections character, for \code{makeMR}, the names of the
##' categories to mark as the dichotomous selections. Required for
##' \code{makeMR}; ignored in \code{makeArray}.
##' @param ... Optional additional attributes to set on the new variable.
##' @return A VariableDefinition that when added to a Dataset will create the
##' categorical-array or multiple-response variable.
##' @export
makeArray <- function (subvariables, dataset=NULL, pattern=NULL, key=namekey(dataset), name, ...) {

    Call <- match.call(expand.dots=FALSE)

    if (missing(name)) {
        halt("Must provide the name for the new variable")
    }

    Call[[1L]] <- as.name("prepareBindInputs")
    subvar_urls <- eval.parent(Call)

    out <- VariableDefinition(subvariables=I(subvar_urls), name=name,
        type="categorical_array", ...)
    return(out)
}

##' Internal function to gather variable URLs for binding
##'
##' Exported only for nonstandard evaluation in makeArray and makeMR
##'
##' @param subvariables a list of Variable objects to bind together, or a
##' Dataset object containing only the Variables to bind (as in from subsetting
##' a Dataset), or values (e.g. names) of variables corresponding to \code{key}.
##' If omitted, must supply \code{dataset} and \code{pattern}. If specifying
##' values, must include \code{dataset}.
##' @param dataset the Crunch Dataset to which the variables in
##' \code{subvariables} belong, or in which to search for variables based
##' on \code{pattern}. If omitted, \code{subvariables} must exist and all
##' Variables in the list must belong to the same Dataset
##' @param pattern An optional regular expression to search for variables to
##' bind within \code{dataset}.
##' @param key character, the name of the Variable field in which to search
##' with \code{pattern}. Default is 'alias'.
##' @param ... additional arguments, which are ignored
##' @return a list with two elements: "dataset" and "variable_urls"
##' @export
##' @keywords internal
prepareBindInputs <- function (subvariables=NULL, dataset=NULL,
                               pattern=NULL, key=namekey(dataset), ...) {

    ## Given inputs to makeArray/makeMR, parse and validate
    listOfVariablesIsValid <- function (lov) {
        return(is.list(lov) && all(vapply(lov, is.variable, logical(1))))
    }
    ## Is this worth validating?
    # datasetURLfromVariables <- function (lov) {
    #     ds_urls <- unique(vapply(lov, datasetReference, character(1)))
    #     if (length(ds_urls) > 1) {
    #         ## see if list of variables actually do belong to same dataset
    #         halt("All variables to be bound together must be from the same dataset")
    #     }
    #     return(ds_urls)
    # }

    if (is.null(dataset)) {
        if (is.dataset(subvariables)) {
            ## as in, if the list of variables is a [ extraction from a Dataset
            variable_urls <- urls(allVariables(subvariables))
        } else if (inherits(subvariables, "VariableCatalog")) {
            variable_urls <- urls(subvariables)
        } else if (listOfVariablesIsValid(subvariables)) {
            variable_urls <- vapply(subvariables, self, character(1))
        } else {
            halt("Must provide a Dataset and either a list of Variables to combine or a pattern to identify Variables within that Dataset")
        }
    } else {
        ## Pattern match (deprecated)
        variable_urls <- findVariableURLs(dataset, refs=subvariables, pattern=pattern, key=key)
    }

    if (!length(variable_urls)) {
        halt("No variables supplied")
    }

    return(variable_urls)
}
