#' Give consent to do things that require permission
#'
#' Potentially destructive actions require that you confirm that you really
#' want to do them. If you're running a script and you know that you want to
#' perform those actions, you can preemptively provide `consent`.
#'
#' @param expr Code to evaluate with consent
#' @return `consent` returns an S3 class "contextManager" object, which
#' you can use with `with`. `with_consent` evaluates its arguments
#' inside the `consent` context.
#' @seealso [`with-context-manager`] [`ContextManager`]
#' @examples
#' \dontrun{
#' with(consent(), delete(ds))
#' # Equivalent to:
#' with_consent(delete(ds))
#' }
#' @export
consent <- function () {
    temp.options(crunch.require.confirmation=FALSE)
}

#' @rdname consent
#' @export
with_consent <- function (expr) {
    with(consent(), eval.parent(expr))
}

askForPermission <- function (prompt="") {
    ## If options explicitly say we don't need to ask, bail.
    ## Have to check that it's FALSE and not NULL. Silence doesn't mean consent.
    must.confirm <- getOption("crunch.require.confirmation", TRUE)
    if (must.confirm == FALSE) return(TRUE)

    ## If we're here but not interactive, we can't give permission.
    if (!is.interactive()) return(FALSE)
    prompt <- paste(prompt, "(y/n) ")
    proceed <- ""
    while (!(proceed %in% c("y", "n"))) {
        proceed <- tolower(read_input(prompt))
    }
    return(proceed == "y")
}

is.interactive <- function () interactive() ## Alias this so that we can mock it out
