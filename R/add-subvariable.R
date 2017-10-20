#' Add subvariable to an array
#'
#' @param variable the array variable to modify
#' @param subvariable the subvariable to add, or a list of those to add, or a
#' dataset subset
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
    ## a mixed or uniform list of variables and subvariables
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
        addVariables(ds, subvariable[vardefs])
        ds <- refresh(ds)
        subvariable <- lapply(subvariable[vardefs], function(x){
            if (is.variable(x)) {
                x
            } else {
                ds[[x$name]]
            }
        })
    }

    # message(all(vapply(subvariable, is.variable, logical(1))))
    ## Get subvariable URL or URLs, depending on how many supplied
    new.urls <- urls(subvariable)

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
