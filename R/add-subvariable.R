#' Add subvariable to an array
#'
#' @param variable the array variable to modify
#' @param subvariable the subvariable to add, or a list of those to add, or a
#' dataset subset. You can supply variables, variable definitions or lists of
#' variables and variable definitions.
#' @return `variable` with the indicated subvariables added.
#' @seealso [subvariables()]
#' @examples
#' \dontrun{
#' ds$allpets <- addSubvariable(ds$allpets, ds$allpets_4)
#' ds$petloc <- addSubvariables(ds$petloc, ds[c("petloc_school", "petloc_daycare")])
#' }
#' @export
addSubvariable <- function(variable, subvariable) {
    stopifnot(is.Array(variable))
    if (!is.derived(variable)) {
        addSubvariablePrimary(variable, subvariable)
    } else {
        addSubvariableDerived(variable, subvariable)
    }
}

#' @rdname addSubvariable
#' @export
addSubvariables <- addSubvariable

addSubvariablePrimary <- function(variable, subvariable) {
    new.urls <- addSubvarDef(variable, subvariable)

    ## Store these for post workaround
    subvar.urls <- subvariableURLs(tuple(variable))

    ## Do the adding
    crPATCH(shojiURL(variable, "catalogs", "subvariables"),
            body = toJSON(sapply(new.urls, emptyObject, simplify = FALSE))
    )

    ## Workaround because apparently bind/rebind isn't retaining the order
    crPATCH(self(variable),
            body = toJSON(list(subvariables = I(c(subvar.urls, new.urls))))
    )

    ## Refresh and return
    dropCache(datasetReference(variable))
    return(invisible(refresh(variable)))
}

addSubvarDef <- function(var, subvar) {
    ## Input can be a variable, subvariable, dataset subset or
    ## a mixed or uniform list of variables and subvariables this
    ## wraps single entries in a list for type consistency.
    if (is.VarDef(subvar) ||
        is.variable(subvar)) {
        ## wrap single variables in list
        subvar <- list(subvar)
    }

    vardefs <- vapply(subvar, is.VarDef, logical(1)) # nolint

    out <- vector("list", length(subvar))

    if (any(vardefs)) {
        ds <- loadDataset(datasetReference(var))
        var_cat_url <- shojiURL(ds, "catalogs", "variables")
        new_var_urls <- lapply(
            subvar[vardefs],
            function(x) try(POSTNewVariable(var_cat_url, x), silent = TRUE)
        )
        checkVarDefErrors(new_var_urls)
        out[vardefs] <- new_var_urls
    }
    if (any(!vardefs)) {
        out[!vardefs] <- urls(subvar[!vardefs])
    }
    return(as.character(out))
}


addSubvariableDerived <- function(variable, subvariable) {
    if (is.VarDef(subvariable) | is.variable(subvariable)) subvariable <- list(subvariable)
    if (is.catalog(subvariable) | is.dataset(subvariable)) subvariable <- lapply(
        seq_along(subvariable),
        function(var_num) subvariable[[var_num]]
    )
    # bypass `derivation(variable)` because `select` zcl function has ids
    # not urls and so absolutifyURL mangles url
    # TODO: use `derivation()` when select has relative urls (pivotal ticket: ???)
    old_deriv <- CrunchExpr(expression = entity(variable)@body$derivation)

    if (isSelectDerivation(old_deriv)) {
        new_deriv <- addToSelectDerivation(old_deriv, subvariable)
    } else if (isSelectCatDerivation(old_deriv)) {
        new_deriv <- addToSelectCatDerivation(old_deriv, subvariable, categories(variable))
    } else {
        halt("Could not add subvariable because did not recognize variable derivation structure")
    }

    derivation(variable) <- new_deriv
    # We don't get metadata from original variable like we would if we were creating
    # subvariable during original derivation...
    # TODO: Also, the alias is really ugly for newly created subvariables
    dropCache(datasetReference(variable))
    return(invisible(refresh(variable)))
}

# If select derivation, can put both existing variables and var defs inside select
isSelectDerivation <- function(deriv) {
    deriv@expression[["function"]] == "array" &&
        deriv@expression[["args"]][[1]][["function"]] %in% c("select", "make_frame")
}

addToSelectDerivation <- function(deriv, new_vars) {
    new_vars <- lapply(new_vars, varUrlOrExpression)

    new_deriv <- deriv
    current_map <- new_deriv@expression$args[[1]]$args[[1]]$map
    # TODO: Better unique strategy
    max_map_name <- max(as.numeric(names(current_map)))

    new_deriv@expression$args[[1]]$args[[1]]$map <- c(
        current_map,
        setNames(new_vars, seq_along(new_vars) + max_map_name)
    )

    new_deriv@expression$args[[1]]$args[[2]]$value <- c(
        new_deriv@expression$args[[1]]$args[[2]]$value,
        lapply(seq_along(new_vars) + max_map_name, as.character)
    )

    new_deriv
}


# if select_cat derivation, existing categorical variables without selections must not add
# any new categories to existing array. var defs cannot be checked but are assumed to also
# have the same categories
# TODO: this does not allow you to add a new subvar where you deliberately choose the
# selected categories which would be nice
isSelectCatDerivation <- function(deriv) {
    deriv@expression[["function"]] == "select_categories" &&
        deriv@expression[["args"]][[1]][["function"]] == "array"  &&
        deriv@expression[["args"]][[1]][["args"]][[1]][["function"]] %in% c("select", "make_frame")
}

addToSelectCatDerivation <- function(deriv, new_vars, existing_cats) {
    new_vars_are_expressions <- vapply(new_vars, is.VarDef, logical(1))
    checkNewSubvarCats(new_vars[!new_vars_are_expressions], existing_cats)
    new_vars <- lapply(new_vars, varUrlOrExpression)

    new_deriv <- deriv
    current_map <- new_deriv@expression$args[[1]]$args[[1]]$args[[1]]$map
    max_map_name <- max(as.numeric(names(current_map)))

    new_deriv@expression$args[[1]]$args[[1]]$args[[1]]$map <- c(
        current_map,
        setNames(new_vars, seq_along(new_vars) + max_map_name
        )
    )

    new_deriv@expression$args[[1]]$args[[1]]$args[[2]]$value <- c(
        new_deriv@expression$args[[1]]$args[[1]]$args[[2]]$value,
        lapply(seq_along(new_vars) + max_map_name, as.character)
    )

    new_deriv
}

varUrlOrExpression <- function(var) {
    if (is.VarDef(var)) {
        out <- var$derivation
        out$references <- var[names(var) != "derivation"]
        out
    } else {
        list(
            variable = self(var),
            references = list(name = name(var))
        )
    }
}

checkNewSubvarCats <- function(vars, cats) {
    new_cat_names <- lapply(vars, function(var) {
        setdiff(names(categories(var)), names(cats))
    })

    if (any(lengths(new_cat_names) > 0)) {
        var_aliases <- vapply(vars[lengths(new_cat_names) > 0], alias, character(1))
        cats_for_vars <- vapply(
            new_cat_names[lengths(new_cat_names) > 0],
            function(cats) paste(cats, collapse = ", "),
            character(1)
        )
        msg <- paste0(
            "Some existing variables have categories not already present in the MR variable, so ",
            "cannot add subvariables.\n  ",
            paste0(var_aliases, "(", cats_for_vars, ")", collapse = ", ")
        )
        halt(msg)
    }
}
