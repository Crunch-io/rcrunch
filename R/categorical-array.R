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

#' Rearrange array subvariables into other configurations
#'
#' Sometimes it is useful to group subvariables across arrays in order to
#' compare them more easily. This function generates a set of derived views of
#' common subvariables across arrays. Because they are derived, they share data
#' with the underlying array variables, and they are thus automatically updated
#' when new data is appended.
#'
#' @param variables List of variables, variable catalog, or dataset subset
#' containing the categorical array or multiple response variables you want to
#' rearrange.
#' @param suffix character string to append to the new variable names. Make it
#' \code{""} if you don't want it to append anything.
#' @return A list of derived VariableDefinitions, one per unique subvariable
#' name across all \code{variables}. Each variable (in \code{variables}) that
#' contains this subvariable will appear
#' as a subvariable in these new derived array definitions. Use
#' \code{addVariables} to add these to your dataset.
#' @examples
#' \dontrun{
#' ds <- addVariables(ds, flipArrays(ds[c("petloc", "petloc2")], suffix=", rearranged"))
#' }
#' @export
flipArrays <- function (variables, suffix=", flipped") {
    ## TODO: validate that all variables are arrays
    ## TODO: validate that they have the same categories? or too rigid?
    ## Get the subvariable catalogs
    if (is.dataset(variables)) {
       variables <- allVariables(variables)
    }
    if (inherits(variables, "VariableCatalog")) {
        ## This feels wrong.
        variables <- lapply(urls(variables),
            function (u) CrunchVariable(variables[[u]]))
    }
    varnames <- vapply(variables, name, character(1))
    subs <- lapply(variables, subvariables)
    subnames <- lapply(subs, names)
    allnames <- unique(unlist(subnames))
    with(temp.option(crunch.namekey.array="name"), {
       ## Use this option so we can extract subvariables by name
       newvars <- lapply(allnames, function (n) {
           has_this_variable <- vapply(subnames, function (x) n %in% x, logical(1))
           vars <- lapply(variables[has_this_variable], function (x) x[[n]])
           deriveArray(subvariables=vars,
               name=paste0(n, suffix),
               subreferences=lapply(varnames[has_this_variable], function (x) list(name=x)))
           })
    })

    ## Return the list of derivations. Can then pass that to addVariables
    return(newvars)
}
