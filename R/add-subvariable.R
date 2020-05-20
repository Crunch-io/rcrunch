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
    } else {
        # bypass `derivation(variable)` because `select` zcl function has ids
        # not urls and so absolutifyURL mangles url
        # TODO: use `derivation()` when select has relative urls (pivotal ticket: ???)
        old_deriv <- CrunchExpr(expression = entity(x)@body$derivation)
        old_vars_catalog <- subvariables(variable)

        new_deriv <- old_deriv
        new_deriv@expression$args[[1]]$args[[1]]$map <- c(
            new_deriv@expression$args[[1]]$args[[1]]$map,
            setNames(
                lapply(subvariable, function(x) list(variable = id(x))),
                seq_along(subvariable) + max(as.numeric(names(old_deriv@expression$args[[1]]$args[[1]]$map)))
            )
        )

        new_deriv@expression$args[[1]]$args[[2]]$value <- c(
            new_deriv@expression$args[[1]]$args[[2]]$value,
            lapply(
                seq_along(subvariable) + max(unlist(as.numeric(new_deriv@expression$args[[1]]$args[[2]]$value))),
                function(x) as.character(x)
            )
        )

        derivation(variable) <- new_deriv
        # We don't get metadata from original variable like we would if we were creating
        # subvariable during original derivation...
        names(subvariables(variable)) <- c(
            names(old_vars_catalog),
            vapply(subvariable, name, character(1))
        )
        descriptions(subvariables(variable)) <- c(
            descriptions(old_vars_catalog),
            vapply(subvariable, description, character(1))
        )
        notes(subvariables(variable)) <- c(
            notes(old_vars_catalog),
            vapply(subvariable, notes, character(1))
        )
        dropCache(datasetReference(variable))
        return(invisible(refresh(variable)))
    }
}

#' @rdname addSubvariable
#' @export
addSubvariables <- addSubvariable

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
