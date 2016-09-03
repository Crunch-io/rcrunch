#' Make a Categorical Array or Multiple Response variable
#'
#' @param subvariables a list of Variable objects to bind together, or a
#' Dataset object containing only the Variables to bind (as in from subsetting
#' a Dataset)
#' @param dataset Argument no longer supported
#' @param name character, the name that the new Categorical Array variable
#' should have. Required.
#' @param selections character, for \code{makeMR}, the names of the
#' categories to mark as the dichotomous selections. Required for
#' \code{makeMR}; ignored in \code{makeArray}.
#' @param ... Optional additional attributes to set on the new variable.
#' @return A VariableDefinition that when added to a Dataset will create the
#' categorical-array or multiple-response variable. \code{deriveArray} will
#' make a derived array expression, while \code{makeArray} and \code{makeMR}
#' return an expression that "binds" variables together, removing them from
#' independent existence.
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
        ## Note that you'll get this error if subvariables is list but one of
        ## its elements is NULL instead of a variable
        halt(dQuote("subvariables"), " cannot be of class ", class(subvariables))
    }

    if (!length(subvariables)) {
        halt("No variables supplied")
    }

    out <- VariableDefinition(subvariables=I(subvariables), name=name,
        type="categorical_array", ...)
    return(out)
}

#' @rdname makeArray
#' @export
deriveArray <- function (subvariables, name, ...) {
    ## Get subvariable URLs
    ## TODO: factor this logic out of here, makeArray, and addSubvariable
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

    subvarids <- as.character(seq_along(subvariables))
    derivation <- zfunc("array", zfunc("select",
        list(map=structure(lapply(subvariables, function (x) list(variable=x)),
            .Names=subvarids)),
        list(value=I(subvarids))))

    return(VariableDefinition(expr=derivation, name=name, ...))
}

flipArrays <- function (variables) {
    ## Assume list of variables. TODO: accept dataset subset

    ## TODO: validate that all variables are arrays
    ## TODO: validate that they have the same categories? or too rigid?

    ## Get the subvariable catalogs
    subs <- lapply(variables, subvariables)
    allnames <- unique(unlist(lapply(subs, names)))

    with(temp.option(crunch.namekey.array="name"), {
        ## Use this option so we can extract subvariables by name
        newvars <- lapply(allnames, function (n) {
            vars <- unlist(lapply(variables, function (x) x[[n]]))
            deriveArray(subvariables=unlist(lapply(vars, self)), name=n,
                subreferences=lapply(vars, function (v) list(name=name(v))))
        })
    })
    ## Return the list of derivations. Can then pass that to addVariables
    return(newvars)
}
