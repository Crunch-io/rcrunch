init.sortCatalog <- function(.Object, ...) {
    ## Sort a ShojiCatalog by the object's "names" (as defined by its names method)
    .Object <- callNextMethod(.Object, ...)
    .Object@index <- .Object@index[order(names(.Object))]
    return(.Object)
}

setMethod("initialize", "ScriptCatalog", init.sortCatalog)

#' Crunch Automation scripts entities for a dataset
#'
#' @param x a CrunchDataset
#' @return an object of class "ScriptCatalog" containing references to
#' Script entities.
#' @name script-catalog
#' @aliases scripts
NULL

#' @rdname script-catalog
#' @export
setMethod("scripts", "CrunchDataset", function(x) {
    ScriptCatalog(crGET(shojiURL(x, "catalogs", "scripts")))
})

#' @rdname crunch-extract
#' @export
setMethod("[[", c("ScriptCatalog", "numeric"), function(x, i, ...) {
    getEntity(x, i, Script, ...)
})

#' @rdname describe-catalog
#' @export
setMethod("timestamps", "ScriptCatalog", function(x) {
    return(from8601(getIndexSlot(x, "creation_time")))
})

#' @rdname describe-catalog
#' @export
setMethod("timestamps", "Script", function(x) {
    return(from8601(x@body$creation_time))
})

#' @rdname describe-catalog
#' @export
setMethod("scriptBody", "ScriptCatalog", function(x) {
    return(getIndexSlot(x, "body"))
})
#' @rdname describe-catalog
#' @export
setMethod("scriptBody", "Script", function(x) {
    return(x@body$body)
})

#' @rdname crunch-is
#' @export
is.script <- function(x) inherits(x, "Script")

#' Get the version saved prior to running a crunch automation script
#'
#' Get the version associated with the dataset right before a crunch
#' automation script was run.
#'
#' @param x A `Script` object
#'
#' @return A version list object that can be used in [`restoreVersion()`]
#' @export
scriptCheckpointVersion <- function(x) {
    stopifnot(is.script(x))
    return(crGET(shojiURL(x, "views", "savepoint"))$body)
}

#' Run a crunch automation script
#'
#' Crunch automation is a custom scripting syntax that allows you to
#' concisely describe the metadata of your data when importing. The
#' syntax is described [in the crunch API documentation](
#' https://docs.crunch.io/feature-guide/feature-automation.html)
#'
#' @param dataset A crunch dataset
#' @param script A path to a text file with crunch automation syntax
#' or a string the syntax loaded in R.
#' @param is_file The default guesses whether a file or string was
#' used in the `script` argument, but you can override the heuristics
#' by specifying `TRUE` for a file, and `FALSE` for a string.
#'
#' @return For `runCrunchAutomation()`: an updated dataset (invisibly),
#' For `crunchAutomationFailure()`, when run after a failure, a list with two items:
#' `script`: that contains the script string sent to the server and `errors` which is a
#' `data.frame` with details about the errors sent from the server.
#' @examples
#' \dontrun{
#' # Can use a path to a file:
#' script_file <- "crunch_automation.txt"
#' ds <- runCrunchAutomation(ds, script_file)
#'
#' # Or a string directly:
#' ds <- runCrunchAutomation(ds, "RENAME v1 TO age;")
#'
#' # After a failed run, some error information prints to console,
#' # But more details are available with function:
#' crunchAutomationFailure()
#'
#' # After a successful, can look at scripts
#' script_info <- scripts(ds)
#' script_info
#'
#' # And even get the version information from the checkpoint before running
#' ds <- restoreVersion(ds, scriptCheckpointVersion(script_info[[1]]))
#'
#'
#' }
#' @export
runCrunchAutomation <- function(dataset, script, is_file = string_is_file_like(script)) {
    stopifnot(is.dataset(dataset))
    stopifnot(is.character(script))
    if (length(script) != 1) halt("Can only run automation on a single script")

    if (is_file) {
        # base R doesn't have a way to read a file as a single string
        script <- paste(readLines(script, encoding = "UTF-8"), collapse = "\n")
    }

    crPOST(
        shojiURL(dataset, "catalogs", "scripts"),
        body = toJSON(wrapEntity(body = list(body = script))),
        status.handlers = list(`400` = crunchAutomationErrorHandler)
    )
    invisible(refresh(dataset))
}


#' @rdname runCrunchAutomation
#' @export
crunchAutomationFailure <- function() {
    as.list(crunch_automation_error_env)
}

string_is_file_like <- function(x) {
    !grepl("\\n", x) & # no new lines
        grepl("\\.[[:alnum:]]+$", x) # ends with a file extension ('.' + any num of letters/nums)
}

# Where we store error information from crunch automation
crunch_automation_error_env <- new.env(parent = emptyenv())


#' @importFrom jsonlite fromJSON
#' @importFrom httr http_status content
crunchAutomationErrorHandler <- function(response) {
    msg <- http_status(response)$message
    automation_messages <- try(content(response)$resolution, silent = TRUE)

    if (!is.error(automation_messages)) {
        # dig into the response to get the script as we sent it to the server
        request_body <- fromJSON(rawToChar(response$request$options$postfields))
        crunch_automation_error_env$script <- request_body$body$body

        # And convert the full information of the error messages into a data.frame
        errors <- lapply(
            automation_messages,
            function(x) as.data.frame(x, stringsAsFactors = FALSE)
        )
        crunch_automation_error_env$errors <- do.call(
            function(x) rbind(x, stringsAsFactors = FALSE),
            errors
        )

        automation_messages <- vapply(
            automation_messages,
            function(e_msg) paste0("\n - ", e_msg$message),
            character(1)
        )

        msg <- paste(
            "Crunch Automation Error. Run `crunchAutomationFailure() for more information.",
            paste(automation_messages, collapse = ""),
            sep = ": "
        )
    }
    halt(msg)
}