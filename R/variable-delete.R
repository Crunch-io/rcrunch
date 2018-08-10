#' Delete Variables Within a Dataset
#'
#' This function permanently deletes a variable from a dataset. For a non-destructive
#' alternative see [hide()].
#'
#' In an interactive session, you will be prompted to confirm that you
#' wish to delete the variable. To avoid that prompt, or to delete variables from a
#' non-interactive session, wrap the call in [with_consent()] to give
#' your permission to delete.
#' @param dataset the Dataset to modify
#' @param variables aliases (following `crunch.namekey.dataset`) or indices
#' of variables to delete.
#' @return (invisibly) `dataset` with the specified variables deleted
#' @seealso [`hide`]
#' @export
deleteVariables <- function(dataset, variables) {
    to.delete <- allVariables(dataset[variables])
    if (length(to.delete) == 1) {
        prompt <- paste0("Really delete ", dQuote(names(to.delete)), "?")
    } else {
        prompt <- paste0(
            "Really delete these ", length(to.delete),
            " variables?"
        )
    }
    if (!askForPermission(prompt)) {
        halt("Must confirm deleting variable(s)")
    }
    out <- lapply(unique(urls(to.delete)), crDELETE)
    dropCache(self(to.delete))
    invisible(refresh(dataset))
}

#' @rdname deleteVariables
#' @export
deleteVariable <- deleteVariables

#' @rdname delete
#' @export
setMethod("delete", "CrunchVariable", function(x, ...) {
    out <- delete(tuple(x), ...)
    dropCache(absoluteURL("../", self(x)))
    invisible(out)
})

#' @rdname delete
#' @export
setMethod("delete", "VariableTuple", function(x, ...) {
    if (!askForPermission(paste0("Really delete ", name(x), "?"))) {
        halt("Must confirm deleting variable")
    }
    out <- crDELETE(self(x))
    invisible(out)
})

#' Delete subvariables from an array
#'
#' Deleting variables requires confirmation. In an interactive session, you will be asked
#' to confirm. To avoid that prompt, or to delete subvariables from a
#' non-interactive session, wrap the call in [with_consent()] to give
#' your permission to delete.
#'
#' To delete the subvariables the function unbinds the array, deletes the subvariable, and
#' then binds the remaining subvariables into a new array.
#' @param variable the array variable
#' @param to.delete aliases (following `crunch.namekey.dataset`) or indices
#' of variables to delete.
#' @return a new version of variable without the indicated subvariables
#' @export
deleteSubvariables <- function(variable, to.delete) {
    ## Identify subvariable URLs
    delete.these <- urls(variable[, to.delete])

    if (length(delete.these) == 1) {
        subvars <- subvariables(variable)
        subvar.urls <- urls(subvars)
        subvar.names <- names(subvars)
        prompt <- paste0(
            "Really delete ",
            dQuote(subvar.names[match(delete.these, subvar.urls)]), "?"
        )
    } else {
        prompt <- paste0(
            "Really delete these ", length(delete.these),
            " variables?"
        )
    }
    if (!askForPermission(prompt)) {
        halt("Must confirm deleting subvariable(s)")
    }

    lapply(delete.these, crDELETE)
    invisible(refresh(variable))
}

#' @rdname deleteSubvariables
#' @export
deleteSubvariable <- deleteSubvariables
