is.error <- function(x) inherits(x, "try-error")

rethrow <- function(x) halt(errorMessage(x))

errorMessage <- function(e) attr(e, "condition")$message

display_console_once <- function(..., option, display_func) {
    # Warn the first time, then set an option so we know not to warn again in
    # the current session
    if (!isTRUE(get_crunch_opt(option))) {
        display_func(...)
        set_crunch_opt(option, TRUE)
    }
}

warn_once <- function(..., call. = FALSE, option) {
    display_console_once(..., call. = call., option = option, display_func = warning)
}

message_once <- function(..., option) {
    display_console_once(..., option = option, display_func = message)
}

vget <- function(name) {
    ## Return a function you can lapply/vapply to select an attribute
    ## Usage: lapply(list.of.stuff, vget("name"))
    ## instead of: lapply(list.of.stuff, function (x) x$name)
    ## N.B.: don't use if you're doing lapply(x, FUN) (because the x's will clash)
    return(function(x) x[[name]])
}

#' Make a prose list
#'
#' Function to paste together a list of items, separated by commas (if more
#' than 2), and with the last one having the collapse string.
#'
#' @param x vector or list
#' @param collapse default="and"
#' @keywords internal
serialPaste <- function(x, collapse = "and") {
    if (length(x) > 1) x[length(x)] <- paste(collapse, x[length(x)])
    join.with <- ifelse(length(x) > 2, ", ", " ")
    return(paste(x, collapse = join.with))
}

now <- function() strftime(Sys.time(), usetz = TRUE)

absoluteURL <- function(urls, base) {
    ## Detect if we have relative urls, and then concatenate if so
    if (length(urls) && !any(startsWith(urls, "http"))) {
        urls <- .abs.urls(urls, base)
    }
    return(urls)
}

#' @importFrom httr parse_url build_url
.abs.urls <- function(urls, base) {
    base.url <- parse_url(base)
    urls <- vapply(urls, function(x, b) {
        b$path <- joinPath(b$path, x)
        if (is.null(b$scheme)) {
            ## If file path and not URL, as in for tests,
            ## let's return it relative
            return(b$path)
        }
        ## Pop off any leading "/" because build_url will add it
        b$path <- sub("^/", "", b$path)
        b$query <- NULL ## Catalog query params aren't valid for entities
        return(build_url(b))
    }, character(1), b = base.url, USE.NAMES = FALSE)
    return(urls)
}

joinPath <- function(base.path, relative.part) {
    if (startsWith(relative.part, "/")) {
        ## This is absolute, relative to the host
        return(relative.part)
    }
    u <- c(strsplit(base.path, "/")[[1]], strsplit(relative.part, "/")[[1]])
    ## Drop any references to current location (.)
    u <- u[u != "."]
    ## Walk the ..
    if (any(u == "..")) {
        ## If we're here, we must have some normalization to do
        i <- 1
        n <- length(u)
        while (i <= n) {
            if (u[i] == "..") {
                ## Remove i and the one before it, and roll the counter back
                u <- u[-c(i - 1, i)]
                n <- n - 2
                i <- i - 1
            } else {
                i <- i + 1
            }
        }
    }
    out <- paste(u, collapse = "/")
    if (endsWith(relative.part, "/")) {
        out <- paste0(out, "/")
    }
    return(out)
}

emptyObject <- function(...) {
    ## toJSON(list()) is "[]". toJSON(emptyObject()) is "{}"
    ##
    ## Make the function take ... so you can *apply over something and just
    ## call the function
    structure(list(), .Names = character(0))
}

I <- function(x) {
    ## Because of R deprecation warning:
    ## Calling 'structure(NULL, *)' is deprecated, as NULL cannot have attributes.
    if (is.null(x)) return(x)
    ## In R 4.1 S4 objects get warning if you try to I() them.
    if (isS4(x)) return(x)

    base::I(x)
}

null <- function(...) NULL

uniquify <- function(str) {
    ## Append (#) to strings to make sure they are unique
    dups <- duplicated(str)
    while (any(dups)) {
        str[dups] <- addUniqueSuffix(str[dups])
        dups <- duplicated(str)
    }
    return(str)
}

addUniqueSuffix <- function(str) {
    ## Add "  (1)" and increment that number if it's already been appended
    already.has <- grepl("  \\([0-9]+\\)$", str)
    suffix <- rep("  (1)", length(str))
    suffix[already.has] <- paste0(
        "  (",
        as.numeric(sub("^(.*)  \\(([0-9]+)\\)$", "\\2", str[already.has])) + 1,
        ")"
    )
    str[already.has] <- sub("^(.*)  \\(([0-9]+)\\)$", "\\1", str[already.has])
    return(paste0(str, suffix))
}

## Borrowed from Hadley
"%||%" <- function(a, b) if (!is.null(a)) a else b


## from future import ...
basefuns <- ls(envir = asNamespace("base"))
alt.startsWith <- function(x, prefix) {
    substr(x, 1, nchar(prefix)) == prefix
}
if (!("startsWith" %in% basefuns)) {
    startsWith <- alt.startsWith
}

alt.endsWith <- function(x, suffix) {
    z <- nchar(x)
    substr(x, z - nchar(suffix) + 1, z) == suffix
}
if (!("endsWith" %in% basefuns)) {
    endsWith <- alt.endsWith
}

vectorOrList <- function(obj, type) {
    if (is.vector(obj) && inherits(obj, type)) {
        return(TRUE)
    }
    if (is.list(obj)) {
        if (all(vapply(obj, inherits, logical(1), what = type))) {
            return(TRUE)
        }
    }
    return(FALSE)
}

# nolint start
#' Get/set options (user-specified, in environment, or in R options)
#'
#' These functions allow for a consistent framework of options for the
#' crunch package. When retrieving options, `envOrOption()` first
#' looks for options set with the `set_crunch_opts()`, followed by
#' options in the environment (see [`Sys.getenv()`])
#' and finally in the R options (see [`options`]).
#'
#' @details
#' Environment variables are generally set at the operating system level,
#' but R does look at a file called `.Renviron` on startup, and you can
#' also set them using the function [`Sys.setenv()`]. Options are generally
#' set using a `options()` function in the `.Rprofile` file, but can be
#' set using that function anywhere.
#'
#' The main `crunch` R package uses the following options (note that
#' the option name is in all capital letters, with "." replaced with
#' "_" and a "R_" prefix when used as an environment variable):
#'
#' | Option name                  | Env variable                   | Default value | Explanation                                                                 |
#' |------------------------------|--------------------------------|---------------|-----------------------------------------------------------------------------|
#' | crunch.api                   | R_CRUNCH_API                   |"https://app.crunch.io/api/"| URL of API to use                                              |
#' | crunch.api.key               | R_CRUNCH_API_KEY               |               | Key to use to authenticate with crunch (see `help('crunch-api-key')`)       |
#' | crunch.show.progress         | R_CRUNCH_SHOW_PROGRESS         | TRUE          | Whether to show progress bars during interactive sessions                   |
#' | crunch.timeout               | R_CRUNCH_TIMEOUT               | 900           | Number of seconds to wait before timing out a request                       |
#' | crunch.show.progress.url     | R_CRUNCH_SHOW_PROGRESS_URL     | FALSE         | Whether to show the URL when checking progress                              |
#' | crunch_retry_wait            | R_CRUNCH_RETRY_WAIT            | 0.1           | Number of seconds to wait before retrying a download                        |
#' | crunch.require.confirmation  | R_CRUNCH_REQUIRE_CONFIRMATION  | TRUE          | Whether to require confirmation for destructive actions (like [`delete()`]) |
#' | crunch.warn.hidden           | R_CRUNCH_WARN_HIDDEN           | TRUE          | Whether to warn when using a hidden variable                                |
#' | crunch.warn.private          | R_CRUNCH_WARN_PRIVATE          | TRUE          | Whether to warn when using a private variable                               |
#' | crunch.names.includes.hidden.private.variables | R_NAMES_INCLUDES_HIDDEN_PRIVATE_VARIABLES| TRUE | Whether to include hidden/private variables from names(ds) |
#' | crunch.order.var.catalog     | R_CRUNCH_ORDER_VAR_CATALOG     | TRUE          | Whether to set the variable catalog in the order of the hierarchical order  |
#' | crunch.default.derived       | R_CRUNCH_DEFAULT_DERIVED.      | TRUE          | Whether to create variables from expressions that are derived (TRUE) or materialized (FALSE) |
#' | crunch.default.project       | R_CRUNCH_DEFAULT_PROJECT       | -             | Path to a project folder to put new datasets in by default.                 |
#' | crunch.delimiter             | R_CRUNCH_DELIMITER             | "/"           | What to use as a delimiter when printing folder paths                       |
#' | crunch.check.updates         | R_CRUNCH_CHECK_UPDATES         | TRUE          | Whether to check for updates to the crunch package                          |
#' | crunch.debug                 | R_CRUNCH_DEBUG                 | FALSE         | Whether to print verbose information for debugging                          |
#' | test.verify.ssl              | R_TEST_VERIFY_SSL              | TRUE          | Whether to verify ssl in curl during crunch tests                           |
#' | crunch.stabilize.query       | R_CRUNCH_STABILIZE_QUERY       | FALSE         | Whether to stabilize JSON objects for saving as `httptest` objects          |
#' | crunch.namekey.dataset       | R_CRUNCH_NAMEKEY_DATASET       | "alias"       | What variable identifier (alias or name) to use for a dataset's variables   |
#' | crunch.namekey.array         | R_CRUNCH_NAMEKEY_ARRAY         | "alias"       | What variable identifier (alias or name) to use for an array's subvariables |
#' | crunch.namekey.variableorder | R_CRUNCH_NAMEKEY_VARIABLEORDER | "name"        | What variable identifier (alias or name) to use for an order's variables    |
#' | use.legacy.tabbook.endpoint  | R_USE_LEGACY_TABBOOK_ENDPOINT  | FALSE         | (Deprecated) Whether to use legacy tabbook endpoint in [`tabBook()`]        |
#'
#' @param opt the option to get/set
#' @param default if the specified option is not set in either the option or as
#' an environment variable, use this instead.
#' @param ... Named arguments describing which options to set
#' @param .source (Optional) A character vector describing where the option was set from
#' @return the value of the option
#'
#' @keywords internal
#' @export
# nolint end
envOrOption <- function(opt, default = NULL, expect_lgl = FALSE, expect_num = FALSE) {

    # First look in CRUNCH_OPTIONS environment
    crunch_opt <- get_crunch_opt(opt)
    if (is_crunch_opt_default(crunch_opt)) {
        return(default)
    } else if (is.function(crunch_opt)){
        crunch_opt <- crunch_opt()
        if (!is.null(crunch_opt)){
            return(crunch_opt)
        }
    } else if (!is.null(crunch_opt)) {
        attributes(crunch_opt) <- NULL
        return(crunch_opt)
    }

    # Next check in environment variables
    envvar.name <- paste0("R_", toupper(gsub(".", "_", opt, fixed = TRUE)))
    envvar <- Sys.getenv(envvar.name)

    if (nchar(envvar)) {
        if (expect_lgl) envvar <- as.logical(envvar)
        if (expect_num) envvar <- as.numeric(envvar)
        return(envvar)
    }

    # Finally, check the R options or use default
    return(getOption(opt, default))
}

CRUNCH_OPT_DEFAULT <- structure(list(), class = "crunch_default_option")
is_crunch_opt_default <- function(x) inherits(x, "crunch_default_option")


envOrOptionSource <- function(opt) {
    envvar.name <- paste0("R_", toupper(gsub(".", "_", opt, fixed = TRUE)))
    crunch_opt <- get_crunch_opt(opt)
    if (!is.null(crunch_opt)) {
        source <- attr(crunch_opt, "source")
        if (is.null(source)) {
            return(paste0("set using `set_crunch_opts(", opt, " = ...)`"))
        }
        return(paste0("set using `", source, "`"))
    }
    if (Sys.getenv(envvar.name) != "") {
        return(paste0("found in environment variable `", envvar.name, "`"))
    }
    if (!is.null(getOption(opt))) {
        return(paste0("found in `options(", opt, " = ...)`"))
    }
    return("unknown source")
}

CRUNCH_OPTIONS <- new.env(parent = emptyenv())

get_crunch_opt <- function(opt) {
    get0(opt, CRUNCH_OPTIONS)
}

set_crunch_opt <- function(opt, value, source = NULL) {
    if (!is.null(source) && !is.null(value)) value <- structure(value, source = source)
    CRUNCH_OPTIONS[[opt]] <- value
}

#' @rdname envOrOption
#' @export
set_crunch_opts <- function(..., .source = NULL) {
    new <- list(...)
    lapply(names(new), function(nm) set_crunch_opt(nm, new[[nm]], .source))
    invisible(new)
}


#' Change which server to point to
#'
#' A convenience function for changing where you want the Crunch package to try
#' to connect to.
#'
#' @param subdomain the subdomain to use
#' @param port on optional port to use
#'
#' @return nothing
#'
#' @examples
#' setCrunchAPI("local", 8080)
#' setCrunchAPI("app")
#' @keywords internal
#' @export
setCrunchAPI <- function(subdomain, port = NULL) {
    if (!is.null(port)) {
        api <- paste0("http://", subdomain, ".crunch.io:", port, "/api/") # nolint
    } else {
        api <- paste0("https://", subdomain, ".crunch.io/api/")
    }
    options(crunch.api = api)
    return(invisible())
}


# halt if variable is an array variable. If callingFunc is provided, provide
# the function that the user used to get to this point.
haltIfArray <- function(variable, callingFunc) {
    # if the variable is not an array type, return quickly
    # TODO: should this also short-circuit if variable is not a variable?
    if (!is.Array(variable)) {
        return(TRUE)
    }

    # Add the callingFunc in, if present
    if (!missing(callingFunc)) {
        halt(
            "Array-like variables can't be used with function `",
            callingFunc, "`."
        )
    }

    # can't determine the function or haltIfArray is being called directly,
    # still error
    halt("Array-like variables can't be used.")
}

#' Check that a value is TRUE or FALSE
#'
#' @param value Value to check
#'
#' @return `TRUE` if `value` is either `TRUE` or `FALSE`, `FALSE` otherwise
#'
#' @keywords internal
is.TRUEorFALSE <- function(value) {
    return(is.logical(value) && length(value) == 1 && !is.na(value))
}
is.singleCharacter <- function(value) {
    return(is.character(value) && length(value) == 1)
}

#' Check if a user has packages installed
#'
#' @param pkgs a character vector of package names to check.
#'
#' @return nothing, called for side effects
#'
#' @keywords internal
checkInstalledPackages <- function(pkgs) {
    installed <- pkgs %in% rownames(utils::installed.packages())
    if (!all(installed)) {
        halt("Missing required packages: ", serialPaste(dQuote(pkgs[!installed])))
    }
}

#' Check if a package has a function installed
#'
#' This is useful for handling if the user has the correct version of a suggested
#' package installed.
#'
#' @param fun a string, name of the function to search for
#' @param pkg a string, the package to search for the function in
#' @keywords internal
#'
#' @return Logical
hasFunction <- function(fun, pkg) {
    fun %in% ls(envir = asNamespace(pkg))
}

#' Escape a regular expression
#'
#' This function takes a string and escapes all of the special characters in the
#' string. For example, the `.` in `VB.NET` will be escaped with a slash (though
#' regular R printing will make it look like there are two slashes).
#'
#' @param string A regular expression to escape
#' @return `string`, escaped.
#' @keywords internal
#' @examples
#' \dontrun{
#' escapeRegex("Tom&Jerry")
#' escapeRegex(".Net")
#' }
escapeRegex <- function(string) {
    out <- gsub("([.|()\\^{}+$*?])", "\\\\\\1", string)
    return(gsub("(\\[|\\])", "\\\\\\1", out))
}

pluralize <- function(string, count) {
    # Naive conditional pluralization
    ifelse(count == 1, string, paste0(string, "s"))
}
