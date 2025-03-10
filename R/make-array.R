#' Make a Categorical Array or Multiple Response variable
#'
#' In most situations we recommend using `deriveArray` which leaves your
#' subvariables in the dataset. `makeArray` _removes_ component subvariables
#' from your dataset. Array variables are composed of a set of "subvariables"
#' bound together for display  in the app. For example, you might have a set of
#' survey questions that ask how the respondent would rate a TV show from 1-5.
#' Array variables allow you to display all of their ratings in a compact table
#' rather than a set of distinct variables.
#'
#' @param subvariables a list of Variable objects to bind together, or a
#' Dataset subset which contains only the Variables to bind.
#' @param name character, the name that the new Categorical Array variable
#' should have.
#' @param selections character (preferred, indicating the names of the
#' categories), or numeric (indicating the IDs of the categories
#' in the combined array, which may not be the same as in the original
#' variables - also note that a category's ID is not the same thing
#' as its `numeric_value`). Required for `makeMR`; optional for
#' `deriveArray`; ignored in `makeArray`.
#' @param numeric Logical indicating whether the array should be a numeric
#' array or categorical array. `NULL` the default will guess numeric if
#' all variables are known to be numeric and categorical if all are
#' categorical. If any subvariables are created from expressions, then
#' their type cannot be guessed and so `numeric` must be specified.
#' @param ... Optional additional attributes to set on the new variable.
#' @return A VariableDefinition that when added to a Dataset will create the
#' categorical-array or multiple-response variable. `deriveArray` will
#' make a derived array expression (or a derived multiple response expression
#' if `selections` are supplied), while `makeArray` and `makeMR`
#' return an expression that "binds" variables together, removing them from
#' independent existence.
#' @examples
#' \dontrun{
#' # Categorical Array - Variables from list of variables
#' ds$enjoy_cat2 <- deriveArray(
#'     list(ds$enjoy1, ds$enjoy2),
#'     "Enjoy activities"
#' )
#'
#' # Categorical Array - Variables from var catalog
#' # (result is the same as `ds$enjoy_cat1` above)
#' ds$enjoy_cat2 <- deriveArray(
#'     ds[c("enjoy1", "enjoy2")],
#'     "Enjoy activities v2"
#' )
#'
#' # Multiple Response (selections as character names)
#' ds$enjoy_mr1 <- deriveArray(
#'     list(ds$enjoy1, ds$enjoy2),
#'     "Enjoy activities very much or a little",
#'     selections = c("Very much", "A little")
#' )
#'
#' # Numeric Array
#' ds$rating_numa <- deriveArray(
#'     list(ds$rating1, ds$rating2),
#'     "Activity Rating"
#' )
#'
#' # Using VarDef to specify metadata (and thus needing to specify type)
#' ds$enjoy_mr <- deriveArray(
#'     list(
#'         VarDef(ds$enjoy1 == "Very much", name = "enjoy brand 1"),
#'         VarDef(ds$enjoy2 == "Very much", name = "enjoy brand 2")
#'     ),
#'     "Enjoy activities with custom names"
#' )
#'
#' # Multiple Response (selections as ids, same as ds$enjoy_mr1)
#' # Be careful `ids(categories(ds$enjoy1))` is not necessarily the same as
#' # `values(categories(ds$enjoy1))`
#' ds$enjoy_mr1 <- deriveArray(
#'     list(ds$enjoy1, ds$enjoy2),
#'     "Enjoy activities very much or a little v2",
#'     selections = c(1, 2)
#' )
#' }
#' @export
deriveArray <- function(subvariables, name, selections, numeric = NULL, ...) {
    expression <- makeFrame(subvariables, numeric)
    if (!missing(selections)) {
        expression <- selectCategories(expression, selections, collapse = FALSE)
    }
    return(VariableDefinition(expression, name = name, ...))
}

#' @rdname deriveArray
#' @export
makeArray <- function(subvariables, name, ...) {
    if (missing(name)) {
        halt("Must provide the name for the new variable")
    }

    if (is.dataset(subvariables)) {
        ## as in, if the list of variables is a [ extraction from a Dataset
        subvariables <- allVariables(subvariables)
    }
    if (!(inherits(subvariables, "VariableCatalog") || is.list(subvariables))) {
        halt("Expected subvariables to be either a variables catalog or list of variables")
    }

    if (!length(subvariables)) {
        halt("No variables supplied")
    }

    subvar_types <- unique(types(subvariables))
    if (all(subvar_types == "numeric")) {
        var_type <- "numeric_array"
    } else if (all(subvar_types %in% c("numeric", "categorical"))) {
        var_type <- "categorical_array"
    } else {
        bad_types <- paste0(
            "'", setdiff(subvar_types, c("numeric", "categorical")), "'", collapse = ", "
        )
        halt("Cannot makeArray from subvariables of type: ", bad_types)
    }

    ## Get subvariable URLs
    subvariables <- urls(subvariables)

    out <- VariableDefinition(
        subvariables = I(subvariables), name = name,
        type = var_type, ...
    )
    return(out)
}

#' @rdname deriveArray
#' @export
makeMR <- function(subvariables, name, selections, ...) {
    if (missing(selections)) {
        halt(
            "Must provide the names of the category or categories that ",
            "indicate the dichotomous selection"
        )
    }

    ## Do `makeArray` to build the definition.
    vardef <- makeArray(
        subvariables = subvariables, name = name,
        selected_categories = I(selections), ...
    )
    vardef$type <- "multiple_response"
    ## We're done. But let's do some validation first.

    ## Get the actual variables so that we can validate
    vars <- lapply(vardef$subvariables, function(u) VariableEntity(crGET(u)))
    are.categorical <- vapply(
        vars,
        function(x) isTRUE(x@body$type == "categorical"), ## Make a type method?
        logical(1)
    )
    if (!all(are.categorical)) {
        varnames <- vapply(
            vars[!are.categorical],
            function(x) x@body$name, ## Make a name() method for VariableEntity
            character(1)
        )
        halt(
            serialPaste(varnames),
            " are not Categorical variables. Convert them to ",
            "Categorical before combining to Multiple Response"
        )
    }

    ## Validate selections before binding
    catnames <- unique(unlist(lapply(vars, function(y) names(categories(y)))))
    if (!all(selections %in% catnames)) {
        halt(
            "Selection(s) not found in variable's categories. ",
            "Category names are: ", serialPaste(catnames)
        )
        ## Could return more useful messaging here
    }

    return(vardef)
}

#' Create Multiple Response Variable from Delimited lists
#'
#' Surveys often record multiple response questions in delimited lists where
#' each respondent's selections are separated by a delimiter like `;` or `|`.
#' This function breaks the delimited responses into subvariables, uploads those
#' subvariables to Crunch, and finally creates a multiple response variable from
#' them.
#'
#' @param var The variable containing the delimited responses
#' @param delim The delimiter separating the responses
#' @param name The name of the resulting MR variable
#' @param selected A character string used to indicate a selection, defaults to
#' "selected"
#' @param not_selected Character string identifying non-selection, defaults to
#' "not_selected"
#' @param unanswered Character string indicating non-response, defaults to NA.
#' @param ... Other arguments to be passed on to [makeMR()]
#'
#' @return a Multiple response variable definition
#' @export
makeMRFromText <- function(var,
                           delim,
                           name,
                           selected = "selected",
                           not_selected = "not_selected",
                           unanswered = NA,
                           ...) {
    if (missing(name)) {
        halt("Must supply a name for the new variable")
    }
    if (is.Text(var)) {
        uniques <- names(table(var))
    } else {
        halt(
            dQuote(deparse(substitute(var))),
            " is of class ", class(var),
            ", it must be a Crunch TextVariable."
        )
    }
    items <- unique(unlist(strsplit(uniques, delim)))
    # make a derivation expression for each unique item
    subvarderivs <- lapply(items, function(x) createSubvarDeriv(
            var, x, delim,
            selected, not_selected, unanswered
        ))
    # mongo errors if there are dots in the names
    names(subvarderivs) <- gsub("\\.", "_", items)

    # generate the ZCL to make an array from the subvariable derivations, and
    # then do selection magic to make an MR
    derivation <- zfunc(
        "select_categories",
        zfunc(
            "array",
            zfunc(
                "select", list(map = subvarderivs),
                list(value = I(c(1, 2, 3, 4, 5)))
            )
        ),
        list(value = I("selected"))
    )

    # hide the original variable
    var <- hide(var)
    # Add default derived behavior
    args <- list(derivation = derivation, name = name, ...)
    if (!"derived" %in% names(args)) args$derived <- derivedVariableDefault()

    return(do.call(VariableDefinition, args))
}

#' Create subvariable derivation expressions
#'
#' This function creates a single subvariable definition based on a character string
#' to search for and an originating variable. It uses regex to determine whether
#' a string is present in a delimited list, then substitutes the user supplied values
#' to indicate selection, non-selection, and missingness.
#'
#'
#' @inheritParams makeMRFromText
#' @param str A string whose presence indicates a selection
#' @keywords internal
#'
#' @return A VariableDefinition
createSubvarDeriv <- function(var, str, delim, selected, not_selected,
                              unanswered) {
    if (is.na(unanswered)) {
        unanswered <- "No Data"
    }
    new_cat_type <- list(
        value = list(
            class = "categorical",
            categories = list(
                list(
                    "id" = 1,
                    "name" = unanswered,
                    "numeric_value" = NA,
                    "missing" = TRUE
                ),
                list(
                    "id" = 2,
                    "name" = selected,
                    "numeric_value" = NA,
                    "missing" = FALSE
                ),
                list(
                    "id" = 3,
                    "name" = not_selected,
                    "numeric_value" = NA,
                    "missing" = FALSE
                )
            )
        )
    )
    new_cat <- list(column = I(1:3), type = new_cat_type)
    deriv <- zfunc("case", new_cat)
    deriv$args[[2]] <- zfunc("is_missing", var)
    deriv$args[[3]] <- zfunc("~=", var, buildDelimRegex(str, delim)) # nolint
    # Mongo doesn't allow aliases with dots
    new_alias <- paste0(alias(var), "_", gsub("\\.", "_", str)) # nolint
    deriv$references <- list(name = str, alias = new_alias)
    return(deriv)
}

#' Build Regex to find  delimited items.
#'
#' A delimited item `maple` can appear in a list in four ways
#' 1. At the start of a list `maple; oak`
#' 1. In the middle of a list `oak; maple; birch`
#' 1. At the end of a list `oak; maple`
#' 1. Alone with no delimiters `maple`
#'
#' This function builds a regex expression which captures those four cases It
#' is mostly broken out of [createSubvarDeriv()] for testing purposes.
#'
#' @inheritParams createSubvarDeriv
#'
#' @return A character string
#' @keywords internal
buildDelimRegex <- function(str, delim) {
    # the delimeter needs to be escaped in case it's a regex character
    delim <- escapeRegex(delim)
    str <- escapeRegex(str)
    regex <- paste0(
        "^", str, delim, "|",
        delim, str, delim, "|",
        delim, str, "$", "|",
        "^", str, "$"
    )
    return(regex)
}

#' @rdname expressions-internal
#' @export
makeFrame <- function(x, numeric = NULL) {
    ## Get subvariable URLs
    if (is.dataset(x)) {
        ## as in, if the list of variables is a [ extraction from a Dataset
        x <- allVariables(x)
    }

    # if it's a list, it could contain variable definitions:
    if (is.list(x) & !is.VarDef(x)) {
        x <- x[lengths(x) > 0] # remove NULLs (from eg slider)
        subvar_types <- vapply(x, function(sv) {
            if (is.VarDef(sv)) return("vardef")
            else if (is.variable(sv)) return(type(sv))
            else return("unknown")
        }, character(1))
        x <- lapply(x, zcl)
    } else if (inherits(x, "VariableCatalog")) {
        # but ShojiCatalogs don't give their urls when lapplying, so treat differently
        subvar_types <- types(x)
        x <- lapply(urls(x), function(sv) list(variable = sv))
    } else {
        halt("Expected a Variable Catalog or a list of Variables/Expressions/VarDefs")
    }

    numeric <- check_make_frame_type_arg(numeric, subvar_types)

    subvarids <- sprintf("%04d", seq_along(x))
    expression <- zfunc("array", zfunc(
        "make_frame",
        list(map = structure(x, .Names = subvarids)),
        list(value = I(subvarids))
    ), numeric = list(value = numeric))
    # TODO: filters are not preserved in makeFrame expressions because
    # they aren't preserved in `VarDefs` which expressions are wrapped in
    # when forming variables... I believe this will only affect someone trying to
    # `as.vector()` an array, which also doesn't currently work so leave it for now.
    CrunchExpr(expression = expression)
}

check_make_frame_type_arg <- function(numeric, subvar_types) {
    if (is.null(numeric)) {
        if (all(subvar_types == "categorical")) {
            numeric <- FALSE
        } else if (all(subvar_types == "numeric")) {
            numeric <- TRUE
        } else {
            halt("Could not guess array type, specify `numeric` argument in `makeFrame()`")
        }
    } else if (!is.logical(numeric) || length(numeric) != 1) {
        halt("Expected `numeric` argument of `makeFrame()` to be TRUE or FALSE")
    }

    numeric
}


#' Rearrange array subvariables
#'
#' Sometimes it is useful to group subvariables across arrays in order to
#' compare them more easily. This function generates a set of derived views of
#' common subvariables across arrays. Because they are derived, they share data
#' with the underlying array variables, and they are thus automatically updated
#' when new data is appended.
#'
#' @param variables List of variables, a variable catalog, or a dataset subset
#' containing the categorical array or multiple response variables you want to
#' rearrange.
#' @param suffix character string to append to the new variable names. Pass
#' `""` if you don't want it to append anything.
#' @return A list of derived VariableDefinitions, one per unique subvariable
#' name across all `variables`. Each variable in `variables` that
#' contains this subvariable will appear as a subvariable in these new derived
#' array definitions. Use [`addVariables`] to add these to your dataset.
#' @examples
#' \dontrun{
#' ds <- addVariables(ds, flipArrays(ds[c("petloc", "petloc2")], suffix = ", rearranged"))
#' }
#' @export
flipArrays <- function(variables, suffix = ", flipped") {
    ## TODO: validate that all variables are arrays
    ## TODO: validate that they have the same categories? or too rigid?
    ## Get the subvariable catalogs
    if (is.dataset(variables)) {
        variables <- allVariables(variables)
    }
    if (inherits(variables, "VariableCatalog")) {
        ## This feels wrong.
        variables <- lapply(
            urls(variables),
            function(u) CrunchVariable(variables[[u]])
        )
    }
    varnames <- vapply(variables, name, character(1))
    subs <- lapply(variables, subvariables)
    subnames <- lapply(subs, names)
    allnames <- unique(unlist(subnames))
    with(temp.option(crunch = list(crunch.namekey.array = "name")), {
        ## Use this option so we can extract subvariables by name
        newvars <- lapply(allnames, function(n) {
            has_this_variable <- vapply(subnames, function(x) n %in% x, logical(1))
            vars <- lapply(variables[has_this_variable], function(x) x[[n]])
            deriveArray(
                subvariables = vars,
                name = paste0(n, suffix),
                subreferences = lapply(
                    varnames[has_this_variable], function(x) list(name = x)
                )
            )
        })
    })

    ## Return the list of derivations. Can then pass that to addVariables
    return(newvars)
}
