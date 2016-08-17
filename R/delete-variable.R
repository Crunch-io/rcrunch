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
