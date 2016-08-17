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
    out <- lapply(urls(to.delete),
        function (x) delete(to.delete[[x]], confirm=FALSE))
    dropCache(self(to.delete))
    invisible(refresh(dataset))
}

#' @rdname deleteVariables
#' @export
deleteVariable <- deleteVariables

#' @rdname delete
#' @export
setMethod("delete", "CrunchVariable", function (x, confirm=requireConsent(), ...) {
    out <- delete(tuple(x), confirm=confirm)
    dropCache(absoluteURL("../", self(x)))
    invisible(out)
})

#' @rdname delete
#' @export
setMethod("delete", "VariableTuple", function (x, confirm=requireConsent(), ...) {
    if (confirm && !askForPermission(paste0("Really delete ", name(x), "?"))) {
        halt("Must confirm deleting variable")
    }
    out <- crDELETE(self(x))
    if (length(out)) {
        ## Array defintion. Returned subvariables. Delete them
        ## TODO: In future, remove this if block when API doesn't do this
        lapply(out, crDELETE)
    }
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
deleteSubvariables <- function (variable, to.delete) {
    ## Store some metadata up front
    payload <- copyVariableReferences(variable)
    subvars <- subvariables(variable)
    subvar.urls <- urls(subvars)
    subvar.names <- names(subvars)

    ## Identify subvariable URLs
    delete.these <- urls(variable[to.delete])
    tryCatch(lapply(delete.these, crDELETE), error=function (e) {
        if (grepl("Please delete the array variable first", e$message)) {
            ## The future isn't here yet.

            ## Unbind
            all.subvar.urls <- unlist(unbind(variable))

            ## Delete
            dels <- lapply(delete.these, function (x) try(crDELETE(x)))

            ## Setdiff those deleted from those returned from unbind
            payload$subvariables <- I(setdiff(all.subvar.urls, delete.these))
            class(payload) <- "VariableDefinition"

            ## Rebind
            new_url <- POSTNewVariable(variableCatalogURL(variable), payload)

            ## Prune subvariable name prefix, or otherwise reset the names
            subvars <- Subvariables(crGET(absoluteURL("subvariables/", new_url)))
            names(subvars) <- subvar.names[match(urls(subvars), subvar.urls)]

            ## Done.
            variable <<- new_url
        } else {
            stop(e$message)
        }
    })
    invisible(variable)
}

#' @rdname deleteSubvariables
#' @export
deleteSubvariable <- deleteSubvariables
