#' Add subvariable to an array
#'
#' @param variable the array variable to modify
#' @param subvariable the subvariable to add, or a list of those to add, or a
#' dataset subset
#' @return \code{variable} with the indicated subvariables added.
#' @examples
#' \dontrun{
#' ds$allpets <- addSubvariable(ds$allpets, ds$allpets_4)
#' ds$petloc <- addSubvariables(ds$petloc, ds[c("petloc_school", "petloc_daycare")])
#' }
#' @export
addSubvariable <- function (variable, subvariable) {
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
    invisible(refresh(variable))
}

#' @rdname addSubvariable
#' @export
addSubvariables <- addSubvariable
