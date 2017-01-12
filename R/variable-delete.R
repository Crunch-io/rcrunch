#' Delete Variables Within a Dataset
#' @param dataset the Dataset to modify
#' @param variables aliases (following \code{crunch.namekey.dataset}) or indices
#' of variables to delete.
#' @param confirm logical: should the user be asked to confirm deletion.
#' Default is \code{TRUE} if in
#' an interactive session. You can avoid the confirmation prompt if you delete
#' \code{with(\link{consent})}.
#' @return (invisibly) \code{dataset} with the specified variables deleted
#' @seealso \code{\link{hide}}
#' @export
deleteVariables <- function (dataset, variables, confirm=requireConsent()) {
    if (!missing(confirm)) {
        warning("The 'confirm' argument is deprecated. See ?with_consent.",
            call.=FALSE)
    }
    to.delete <- allVariables(dataset[variables])
    if (length(to.delete) == 1) {
        prompt <- paste0("Really delete ", dQuote(names(to.delete)), "?")
    } else {
        prompt <- paste0("Really delete these ", length(to.delete),
            " variables?")
    }
    if (confirm && !askForPermission(prompt)) {
        halt("Must confirm deleting variable(s)")
    }
    out <- lapply(urls(to.delete), crDELETE)
    dropCache(self(to.delete))
    invisible(refresh(dataset))
}

#' @rdname deleteVariables
#' @export
deleteVariable <- deleteVariables

#' @rdname delete
#' @export
setMethod("delete", "CrunchVariable", function (x, ...) {
    out <- delete(tuple(x), ...)
    dropCache(absoluteURL("../", self(x)))
    invisible(out)
})

#' @rdname delete
#' @export
setMethod("delete", "VariableTuple", function (x, confirm=requireConsent(), ...) {
    if (!missing(confirm)) {
        warning("The 'confirm' argument is deprecated. See ?with_consent.",
            call.=FALSE)
    }
    if (confirm && !askForPermission(paste0("Really delete ", name(x), "?"))) {
        halt("Must confirm deleting variable")
    }
    out <- crDELETE(self(x))
    invisible(out)
})

#' Delete subvariables from an array
#'
#' This function conceals the dirty work in making this happen. The array
#' gets unbound, the subvariables deleted, and then the remaining subvariable
#' are rebound into a new array.
#' @param variable the array variable
#' @param to.delete aliases (following \code{crunch.namekey.dataset}) or indices
#' of variables to delete.
#' @param confirm logical: should the user be asked to confirm deletion.
#' Default is \code{TRUE} if in
#' an interactive session. You can avoid the confirmation prompt if you delete
#' \code{with(\link{consent})}.
#' @return a new version of variable without the indicated subvariables
#' @export
deleteSubvariables <- function (variable, to.delete, confirm=requireConsent()) {
    if (!missing(confirm)) {
        warning("The 'confirm' argument is deprecated. See ?with_consent.",
            call.=FALSE)
    }
    ## Identify subvariable URLs
    delete.these <- urls(variable[to.delete])

    if (confirm) {
        ## Get confirmation
        if (length(delete.these) == 1) {
            subvars <- subvariables(variable)
            subvar.urls <- urls(subvars)
            subvar.names <- names(subvars)
            prompt <- paste0("Really delete ",
                dQuote(subvar.names[match(delete.these, subvar.urls)]), "?")
        } else {
            prompt <- paste0("Really delete these ", length(delete.these),
                " variables?")
        }
        if (!askForPermission(prompt)) {
            halt("Must confirm deleting subvariable(s)")
        }
    }

    lapply(delete.these, crDELETE)
    invisible(refresh(variable))
}

#' @rdname deleteSubvariables
#' @export
deleteSubvariable <- deleteSubvariables
