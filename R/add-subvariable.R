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
    ## Input can be a variable, subvariable, dataset subset or
    ## a mixed or uniform list of variables and subvariables this
    ## wraps single entries in a list for type consistency.
    if (inherits(subvariable, "VariableDefinition") ||
            is.variable(subvariable)) {
        ## wrap single variables in list
        subvariable <- list(subvariable)
    }
    vardefs <- vapply(subvariable,
        function(x) inherits(x, "VariableDefinition"),
        logical(1))

    if (any(vardefs)) {
        ds <- loadDataset(datasetReference(variable))
        ds <- addVariables(ds, subvariable[vardefs])

        ## Since this function accepts mixed lists of variables and definitions
        ## we need to replace the definitions with the variables once they've been added.
        subvariable[vardefs] <- lapply(subvariable[vardefs], function(x){
            if (is.variable(x)) {
                x
            } else {
                ds[[x$name]]
            }
        })
    }

    ## There is some inconsistency in the output order after patching the subvariables list
    ## this stores the proper order.
    order <- c(names(subvariables(variable)), vapply(subvariable, name, ""))

    ## Get subvariable URL or URLs, depending on how many supplied
    new.urls <- urls(subvariable)
    ## Do the adding
    crPATCH(shojiURL(variable, "catalogs", "subvariables"),
        body=toJSON(sapply(new.urls, emptyObject, simplify=FALSE)))
    variable <- refresh(variable)
    subvariables(variable) <- subvariables(variable)[order]
    ## Refresh and return
    dropCache(datasetReference(variable))
    return(invisible(refresh(variable)))
}

#' @rdname addSubvariable
#' @export
addSubvariables <- addSubvariable
