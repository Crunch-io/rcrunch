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
setMethod("scriptBody", "Script", function(x) {
    return(x@body$body)
})

#' @rdname crunch-is
#' @export
is.script <- function(x) inherits(x, "Script")


#' @rdname automation-undo
#' @export
setMethod("undoScript", c("CrunchDataset", "Script"), function(dataset, x) {
    crDELETE(shojiURL(x, "catalogs", "output"))
    invisible(refresh(dataset))
})

#' @rdname automation-undo
#' @export
setMethod("undoScript", c("CrunchDataset", "ANY"), function(dataset, x) {
    undoScript(dataset, scripts(dataset)[[x]])
})

#' @rdname automation-undo
#' @export
setMethod("revertScript", c("CrunchDataset", "Script"), function(dataset, x) {
    crPOST(shojiURL(x, "fragments", "revert"))
    invisible(refresh(dataset))
})

#' @rdname automation-undo
#' @export
setMethod("revertScript", c("CrunchDataset", "ANY"), function(dataset, x) {
    revertScript(dataset, scripts(dataset)[[x]])
})

#' @rdname automation-undo
#' @export
setMethod("scriptSavepoint", "Script", function(x) {
    return(crGET(shojiURL(x, "views", "savepoint"))$body)
})

#' Run a crunch automation script
#'
#' Crunch Automation is a custom scripting language that allows you to
#' execute common Crunch commands. The syntax is described in the
#' [Crunch API documentation](
#' https://help.crunch.io/hc/en-us/categories/360004356012-Crunch-Automation).
#'
#' If a character vector with length bigger than 1 is passed to `script`,
#' it's converted to a string by concatenating its elements together using
#' line breaks.
#'
#' @param x A crunch dataset or project folder (for backwards compatibility,
#' `dataset` is also accepted)
#' @param script A path to a text file containing a Crunch Automation script
#' or a character vector of length 1 or more with Crunch Automation commands (see `Details`)
#' @param is_file The default guesses whether a file or string was
#' used in the `script` argument, but you can override the heuristics
#' by specifying `TRUE` for a file, and `FALSE` for a string.
#' @param encoding Optional encoding to convert **from**, defaults to UTF-8.
#' The API accepts only UTF-8, so all text will be converted to UTF-8 before
#' being sent to the server.
#' @param strict_subvariable_syntax Whether to require referring to axes using
#' "bracket" notation (for example `satisfaction[discounts]`). Defaults to `TRUE`.
#' If `FALSE`, refer to axis members by their codes only, which can cause problems
#' when axes have the same codes across different arrays.
#' @param ... Additional options, such as `dry_run = TRUE`, passed on
#' to the API if x is a dataset (if x is a project folder, an error is thrown)
#' @return For `runCrunchAutomation()`: an updated dataset/project folder (invisibly),
#' For `showScriptErrors()`, when run after a failure, a list with two items:
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
#' # A "dry run" that validates the script but does not run it:
#' runCrunchAutomation(ds, "RENAME V1 TO age;", dry_run = TRUE)
#'
#' # After a failed run, some error information prints to console,
#' # But more details are available with function:
#' showScriptErrors()
#'
#' # After a successful run, can look at scripts:
#' scripts(ds)
#'
#' # Run Crunch Automation on a folder:
#' my_folder <- cd(projects(), "folder1")
#' runCrunchAutomation(my_folder, 'CREATE FOLDER "folder2";')
#' }
#' @seealso [`automation-undo`] & [`script-catalog`]
#' @export
runCrunchAutomation <- function(
    x,
    script,
    is_file = string_is_file_like(script),
    encoding = "UTF-8",
    ...,
    strict_subvariable_syntax = TRUE) {
    # a previous version of this function had `dataset` as its first argument
    # because the function is now broader, the argument is called `x`
    # the following block is for backwards compatibility
    args <- as.list(sys.call())
    args[[1]] <- NULL
    if ("dataset" %in% names(args)) {
        warning(
            "The argument `dataset` has been renamed to `x` because the function ",
            "now supports more generic operations. ",
            "While using `dataset` will continue to work, it will emit this warning."
        )
        target <- list(x = args$dataset)
        args$dataset <- NULL
        out <- do.call(
            "runCrunchAutomation", c(target, args),
            quote = FALSE, envir = parent.frame()
        )
        return(out)
    }

    filename <- NULL
    if (is_file) {
        filename <- script
        script <- readLines(script, encoding = encoding, warn = FALSE)
    }
    script <- paste(script, collapse = "\n")
    reset_automation_error_env()
    automation_error_env$file <- filename
    automation_error_env$last_attempted_script <- script

    sendCrunchAutomationScript(x = x, script = script, ..., strict_subvariable_syntax = strict_subvariable_syntax)

    # Provide feedback for dry_run success so that user is confident it was successful
    if (isTRUE(list(...)$dry_run)) {
        message("Script dry run was successful")
        return(invisible(x))
    }

    invisible(refresh(x))
}

setMethod("sendCrunchAutomationScript", "CrunchDataset", function(x,
                                                                  script,
                                                                  ...) {
    crPOST(
        shojiURL(x, "catalogs", "scripts"),
        body = toJSON(wrapEntity(body = list(body = script, ...))),
        status.handlers = list(`400` = crunchAutomationErrorHandler),
        progress.handler = crunchAutomationErrorHandler,
        config = add_headers(`Content-Type` = "application/json")
    )

    invisible(NULL)
})

setMethod(
    "sendCrunchAutomationScript",
    "ProjectFolder",
    function(
        x,
        script,
        is_file = string_is_file_like(script),
        encoding = "UTF-8",
        ...
    ) {
        # project folders include a slot views with element execute,
        # which gives us the URL to hit;
        # but the account ('top-level folder', what you get from: `projects()`)
        # is also of class ProjectFolder, but doesn't include this info;
        # running CA scripts on the account is not supported currently
        if (!is.crunchURL(x@views$execute)) {
            halt(
                "This folder does not support Crunch Automation scripts at this time."
            )
        }

        dots <- list(...)
        dots[["strict_subvariable_syntax"]] <- NULL # Remove this, waiting on
        # https://crunchio.atlassian.net/browse/AT-526?atlOrigin=eyJpIjoiODQzODlhYzlhOGE1NDM4YWI3Zjk0MTMwMzJlOTczNjAiLCJwIjoiaiJ9
        if (length(dots) > 0) {
            # could have been a warning, but went with error in case a user
            # would try running a destructive operation with dry_run = TRUE
            stop("extra arguments (...) are not supported when x is a ProjectFolder")
        }

        crPOST(
            shojiURL(x, "views", "execute"),
            body = toJSON(wrapView(value = script)),
            status.handlers = list(`400` = crunchAutomationErrorHandler),
            progress.handler = crunchAutomationErrorHandler,
            config = add_headers(`Content-Type` = "application/json")
        )

        invisible(NULL)
    }
)

string_is_file_like <- function(x) {
    length(x) == 1 && # length 1 string
        !grepl("\\n", x) && # no new lines
        # ends with a file extension ('.' + any num of letters/nums) or exists
        (grepl("\\.[[:alnum:]]+$", x) || file.exists(x))
}

# Where we store error information from crunch automation
automation_error_env <- new.env(parent = emptyenv())

reset_automation_error_env <- function() {
    rm(list = ls(envir = automation_error_env), envir = automation_error_env)
}

#' @rdname runCrunchAutomation
#' @export
showScriptErrors <- function() {
    out <- as.list(automation_error_env)

    if (is.null(out) || is.null(out$errors)) {
        return(invisible(out))
    }

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

# nocov start
make_rstudio_markers <- function(errors) {
    markers <- data.frame(
        type = "error",
        file = errors$file,
        line = ifelse(is.na(errors$errors$line), 1, errors$errors$line),
        column = ifelse(is.na(errors$errors$column), 1, errors$errors$column),
        message = errors$errors$message,
        stringsAsFactors = FALSE
    )
    rstudioapi::sourceMarkers("crunchAutomation", markers)
}
# nocov end

#' @importFrom jsonlite fromJSON
#' @importFrom httr http_status content
crunchAutomationErrorHandler <- function(response) {
    # Get data from appropriate place if it's a direct httr response
    # or if it's from pollProgress (because validation was async)
    # TODO: If this becomes a common pattern, should probably put
    # somewhere else
    if (inherits(response, "response")) {
        msg <- http_status(response)$message
        response_content <- content(response)
    } else {
        msg <- response$message$description
        response_content <- response$message
    }
    automation_messages <- try(response_content$resolution, silent = TRUE)

    if (!is.error(automation_messages) && !is.null(automation_messages)) {
        automation_error_env$script <- automation_error_env$last_attempted_script

        # convert the full information of the error messages into a data.frame
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

        error_items <- automation_errors_text(errors, 5)

        # only provides more information if truncated or if RStudio source
        # markers are available
        if (
            attr(error_items, "truncated") ||
                (!is.null(automation_error_env$file) && rstudio_markers_available())
        ) {
            more_info_text <- "\n\nRun command `showScriptErrors()` for more information."
        } else {
            more_info_text <- NULL
        }

        msg <- paste("Crunch Automation Error\n", error_items, more_info_text, sep = "")
    } else {
        # could also have information in message property
        # try to use it if it's a character string
        other_msg <- try(content(response)[["message"]], silent = TRUE) # nocov
        if (is.character(other_msg)) { # nocov
            msg <- paste0(msg, " - ", paste0(other_msg, collapse = "\n")) # nocov
        }
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
        out <- paste0(
            out,
            "\n - ... (Showing first ", display_num, " of ", orig_num_errors, " errors)"
        )
        attr(out, "truncated") <- TRUE
    } else {
        attr(out, "truncated") <- FALSE
    }
    out
}
