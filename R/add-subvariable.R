#' Add subvariable to an array
#'
#' @param variable the array variable to modify
#' @param subvariable the subvariable to add, or a list of those to add, or a
#' dataset subset. You can supply variables, variable definitions or lists of
#' variables and variable definitions.
#' @return `variable` with the indicated subvariables added.
#' @seealso [`subvariables`]
#' @examples
#' \dontrun{
#' ds$allpets <- addSubvariable(ds$allpets, ds$allpets_4)
#' ds$petloc <- addSubvariables(ds$petloc, ds[c("petloc_school", "petloc_daycare")])
#' }
#' @export
addSubvariable <- function (variable, subvariable) {

    new.urls <- addSubvarDef(variable, subvariable)

    ## Store these for post workaround
    subvar.urls <- subvariables(tuple(variable))

    ## Do the adding
    crPATCH(shojiURL(variable, "catalogs", "subvariables"),
        body=toJSON(sapply(new.urls, emptyObject, simplify=FALSE)))

    ## Workaround because apparently bind/rebind isn't retaining the order
    crPATCH(self(variable),
        body=toJSON(list(subvariables=I(c(subvar.urls, new.urls)))))

    ## Refresh and return
    dropCache(datasetReference(variable))
    return(invisible(refresh(variable)))
}

#' @rdname addSubvariable
#' @export
addSubvariables <- addSubvariable

addSubvarDef <- function (var, subvar) {
    ## Input can be a variable, subvariable, dataset subset or
    ## a mixed or uniform list of variables and subvariables this
    ## wraps single entries in a list for type consistency.
    if (inherits(subvar, "VariableDefinition") ||
            is.variable(subvar)) {
        ## wrap single variables in list
        subvar <- list(subvar)
    }

    vardefs <- vapply(subvar,
        function(x) inherits(x, "VariableDefinition"),
        logical(1))

    out <- vector("list", length(subvar))

    if (any(vardefs)) {
        ds <- loadDataset(datasetReference(var))
        var_cat_url <- shojiURL(ds, "catalogs", "variables")
        new_var_urls <- lapply(subvar[vardefs],
            function (x) try(POSTNewVariable(var_cat_url, x), silent = TRUE)
        )
        checkVarDefErrors(new_var_urls)
        out[vardefs] <- new_var_urls
    }
    if (any(!vardefs)) {
        out[!vardefs] <- urls(subvar[!vardefs])
    }
    return(as.character(out))
}