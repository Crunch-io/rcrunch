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
#' @seealso [`runCrunchAutomation()`] & [`automation-undo`]
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


#' Undo behavior of a Crunch Automation Script
#'
#' There are two ways to revert the output of a script:
#' - `undoScript()` - A "softer" delete of a script's created artifacts and variables, or
#' - `revertScript()` - A "harder" revert that returns the dataset to the state it was before
#'   running such script.
#'
#' The difference between both is that a hard revert restores the dataset, as it drops all
#' ensuing scripts and their output (artifacts and variables), while an undo only deletes the
#' artifacts and variables created by this script, but changes made by other scripts and this
#' script's record will remain in place.
#'
#' The function `scriptSavepoint()` gets the version object
#'
#' @param x A `Script`, `ScriptCatalog`, or `CrunchDataset` object
#' @param index Index of script to use (if `x` is not a script)
#'
#' @return For `undoScript()` and `revertSctipt()`, invisibly return the updated dataset.
#' For `scriptSavePoint()` a version list object that can be used in [`restoreVersion()`].
#' @export
#' @name automation-undo
#' @seealso [`runCrunchAutomation()`] & [`script-catalog`]
#' @export
setMethod("undoScript", "Script", function(x, ...) {
    crDELETE(self(x))
    invisible(refresh(dataset))
})

#' @rdname automation-undo
#' @export
setMethod("undoScript", "ScriptCatalog", function(x, index, ...) {
    undoScript(x[[index]])
})

#' @rdname automation-undo
#' @export
setMethod("undoScript", "CrunchDataset", function(x, index, ...) {
    undoScript(scripts(x)[[index]])
})

#' @rdname automation-undo
#' @export
setMethod("revertScript", "Script", function(x, ...) {
    crPOST(shojiURL(x, "fragments", "revert"))
    invisible(refresh(dataset))
})

#' @rdname automation-undo
#' @export
setMethod("revertScript", "ScriptCatalog", function(x, index, ...) {
    revertScript(x[[index]])
})

#' @rdname automation-undo
#' @export
setMethod("revertScript", "CrunchDataset", function(x, index, ...) {
    revertScript(scripts(x)[[index]])
})

#' @rdname automation-undo
#' @export
setMethod("scriptSavepoint", "Script", function(x, ...) {
    return(crGET(shojiURL(x, "views", "savepoint"))$body)
})

#' @rdname automation-undo
#' @export
setMethod("scriptSavepoint", "ScriptCatalog", function(x, index, ...) {
    scriptSavepoint(x[[index]])
})

#' @rdname automation-undo
#' @export
setMethod("scriptSavepoint", "CrunchDataset", function(x, index, ...) {
    scriptSavepoint(scripts(x)[[index]])
})

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
#' # After a successful run, can look at scripts
#' scripts(ds)
#'
#' }
#' @export
#' @seealso [`automation-undo`] & [`script-catalog`]
runCrunchAutomation <- function(dataset, script, is_file = string_is_file_like(script)) {
    reset_automation_error_env()
    stopifnot(is.dataset(dataset))
    stopifnot(is.character(script))
    if (length(script) != 1) halt("Can only run automation on a single script")

    if (is_file) {
        automation_error_env$file <- script
        script <- paste(readLines(script, encoding = "UTF-8", warn = FALSE), collapse = "\n")
    } else {
        automation_error_env$file <- NULL
    }

    crPOST(
        shojiURL(dataset, "catalogs", "scripts"),
        body = toJSON(wrapEntity(body = list(body = script))),
        status.handlers = list(`400` = crunchAutomationErrorHandler)
    )
    invisible(refresh(dataset))
}

string_is_file_like <- function(x) {
    !grepl("\\n", x) & # no new lines
        grepl("\\.[[:alnum:]]+$", x) # ends with a file extension ('.' + any num of letters/nums)
}

# Where we store error information from crunch automation
automation_error_env <- new.env(parent = emptyenv())

reset_automation_error_env <- function() {
    rm(list = ls(envir = automation_error_env), envir = automation_error_env)
}

#' @rdname runCrunchAutomation
#' @export
crunchAutomationFailure <- function() {
    out <- as.list(automation_error_env)

    if (is.null(out) || is.null(out$errors)) return(invisible(out))

    if (!is.null(out$file) && rstudio_markers_available()) {
        make_rstudio_markers(out)
    } else {
        message(automation_errors_text(out$errors))
    }
    invisible(out)
}

rstudio_markers_available <- function() {
    requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::hasFun("sourceMarkers")
}

make_rstudio_markers <- function(automation_errors) {
    markers <- data.frame(
        type = "error",
        file = automation_errors$file,
        line = automation_errors$errors$line,
        column = ifelse(is.na(automation_errors$errors$column), 1, automation_errors$errors$column),
        message = automation_errors$errors$message
    )
    rstudioapi::sourceMarkers("crunchAutomation", markers)
}

#' @importFrom jsonlite fromJSON
#' @importFrom httr http_status content
crunchAutomationErrorHandler <- function(response) {
    msg <- http_status(response)$message
    automation_messages <- try(content(response)$resolution, silent = TRUE)

    if (!is.error(automation_messages)) {
        # dig into the response to get the script as we sent it to the server
        request_body <- fromJSON(rawToChar(response$request$options$postfields))
        automation_error_env$script <- request_body$body$body

        # And convert the full information of the error messages into a data.frame
        automation_error_cols <- c("column", "command", "line", "message")
        errors <- lapply(
            automation_messages,
            function(x) {
                # ensure consistent columns
                out <- lapply(automation_error_cols, function(col) x[[col]] %||% NA)
                setNames(as.data.frame(out, stringsAsFactors = FALSE), automation_error_cols)
            }
        )

        errors <- do.call(
            function(...) rbind(..., stringsAsFactors = FALSE),
            errors
        )

        automation_error_env$errors <- errors

        msg <- paste(
            "Crunch Automation Error\n",
            automation_errors_text(errors, 5),
            "\n\nRun command `crunchAutomationFailure()` for more information.",
            sep = ""
        )
    }
    halt(msg)
}

automation_errors_text <- function(errors, display_num = Inf) {
    orig_num_errors <- nrow(errors)
    if (orig_num_errors - display_num > 0) {
        errors <- errors[seq_len(display_num), ]
    }

    out <- paste0(
        " - ",
        ifelse(is.na(errors$line), "", paste0("(line ", errors$line, ") ")),
        errors$message,
        collapse = "\n"
    )

    if (orig_num_errors - display_num > 0) {
        out <- paste0(out, "\n - ... (Showing first ", display_num, " of ", orig_num_errors, " errors)")
    }
    out
}
