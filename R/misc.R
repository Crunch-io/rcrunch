is.error <- function (x) inherits(x, "try-error")

rethrow <- function (x) halt(errorMessage(x))

errorMessage <- function (e) attr(e, "condition")$message

vget <- function (name) {
    ## Return a function you can lapply/vapply to select an attribute
    ## Usage: lapply(list.of.stuff, vget("name"))
    ## instead of: lapply(list.of.stuff, function (x) x$name)
    ## N.B.: don't use if you're doing lapply(x, FUN) (because the x's will clash)
    return(function (x) x[[name]])
}

#' Make a prose list
#'
#' Function to paste together a list of items, separated by commas (if more
#' than 2), and with the last one having the collapse string.
#'
#' @param x vector or list
#' @param collapse default="and"
#' @keywords internal
serialPaste <- function (x, collapse="and") {
	if (length(x)>1) x[length(x)] <- paste(collapse, x[length(x)])
	join.with <- ifelse(length(x)>2, ", ", " ")
	return(paste(x, collapse=join.with))
}

now <- function () strftime(Sys.time(), usetz=TRUE)

absoluteURL <- function (urls, base) {
    ## Detect if we have relative urls, and then concatenate if so
    if (length(urls) && !any(startsWith(urls, "http"))) {
        urls <- .abs.urls(urls, base)
    }
    return(urls)
}

#' @importFrom httr parse_url build_url
.abs.urls <- function (urls, base) {
    base.url <- parse_url(base)
    urls <- vapply(urls, function (x, b) {
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
    }, character(1), b=base.url, USE.NAMES=FALSE)
    return(urls)
}

joinPath <- function (base.path, relative.part) {
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
                u <- u[-c(i-1, i)]
                n <- n - 2
                i <- i - 1
            } else {
                i <- i + 1
            }
        }
    }
    out <- paste(u, collapse="/")
    if (endsWith(relative.part, "/")) {
        out <- paste0(out, "/")
    }
    return(out)
}

emptyObject <- function (...) {
    ## toJSON(list()) is "[]". toJSON(emptyObject()) is "{}"
    ##
    ## Make the function take ... so you can *apply over something and just
    ## call the function
    structure(list(), .Names=character(0))
}

I <- function (x) {
    ## Because of R deprecation warning:
    ## Calling 'structure(NULL, *)' is deprecated, as NULL cannot have attributes.
    if (!is.null(x)) x <- base::I(x)
    return(x)
}

null <- function (...) NULL

uniquify <- function (str) {
    ## Append (#) to strings to make sure they are unique
    dups <- duplicated(str)
    while(any(dups)) {
        str[dups] <- addUniqueSuffix(str[dups])
        dups <- duplicated(str)
    }
    return(str)
}

addUniqueSuffix <- function (str) {
    ## Add "  (1)" and increment that number if it's already been appended
    already.has <- grepl("  \\([0-9]+\\)$", str)
    suffix <- rep("  (1)", length(str))
    suffix[already.has] <- paste0("  (",
        as.numeric(sub("^(.*)  \\(([0-9]+)\\)$", "\\2", str[already.has])) + 1,
        ")")
    str[already.has] <- sub("^(.*)  \\(([0-9]+)\\)$", "\\1", str[already.has])
    return(paste0(str, suffix))
}

## Borrowed from Hadley
"%||%" <- function (a, b) if (!is.null(a)) a else b


## from future import ...
basefuns <- ls(envir=asNamespace("base"))
alt.startsWith <- function (x, prefix) {
    substr(x, 1, nchar(prefix)) == prefix
}
if (!("startsWith" %in% basefuns)) {
    startsWith <- alt.startsWith
}

alt.endsWith <- function (x, suffix) {
    z <- nchar(x)
    substr(x, z - nchar(suffix) + 1, z) == suffix
}
if (!("endsWith" %in% basefuns)) {
    endsWith <- alt.endsWith
}

vectorOrList <- function (obj, type) {
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

#' Grab either env variable or option
#'
#' .Rprofile options are like "crunch.api", while env vars are "R_CRUNCH_API".
#' This function will use the environment variable if it is found, otherwise
#' it looks for the R-based option value.
#'
#' @param opt the option to get
#' @param default if the specified option is not set in either the option or as
#' an environment variable, use this instead.
#'
#' @return the value of the option
#'
#' @keywords internal
#' @export
envOrOption <- function (opt, default = NULL) {
    envvar.name <- paste0("R_", toupper(gsub(".", "_", opt, fixed=TRUE)))
    envvar <- Sys.getenv(envvar.name)

    if (nchar(envvar)) {
        ## Let environment variable override .Rprofile, if defined
        return(envvar)
    } else {
        return(getOption(opt, default))
    }
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
#'
#' @keywords internal
#' @export
setCrunchAPI <- function (subdomain, port=NULL) {
    if (!is.null(port)) {
        api <- paste0("http://", subdomain, ".crunch.io:", port, "/api/")
    } else {
        api <- paste0("https://", subdomain, ".crunch.io/api/")
    }
    options(crunch.api=api)
    return(invisible())
}


# halt if variable is an array variable. If callingFunc is provided, provide the function that
# the user used to get to this point.
haltIfArray <- function (variable, callingFunc) {
    # if the variable is not an array type, return quickly
    # TODO: should this also short-circuit if variable is not a variable?
    if (!is.Array(variable)) {
        return(TRUE)
    }

    # Add the callingFunc in, if present
    if (!missing(callingFunc)) {
        halt("Array-like variables can't be used with function `",
             callingFunc, "`.")
    }

    # can't determine the function or haltIfArray is being called directly,
    # still error
    halt("Array-like variables can't be used.")
}

# validate that rollup resolutions are what are allowed by Crunch
validateResolution <- function (resolution) {
    valid_res <- c("Y", "Q", "M", "W", "D", "h", "m", "s", "ms")

    if (!is.null(resolution) && !(resolution %in% valid_res)) {
        halt(dQuote("resolution"), " is invalid. Valid values are ",
             serialPaste(valid_res, collapse = "or"))
    }
}

# default formats for various resolutions
datetimeFormater <- function (resolution) {
    validateResolution(resolution)
    formats <- list("Y" = "%Y",
                    # there is no %q in python strftime, so can't print quarters
                    "Q" = "%Y-%m-%d",
                    "M" = "%Y-%m",
                    "W" = "%Y W%W",
                    "D" = "%Y-%m-%d",
                    "h" = "%Y-%m-%d %H:00",
                    "m" = "%Y-%m-%d %H:%M",
                    "s" = "%Y-%m-%d %H:%M:%S",
                    "ms" = "%Y-%m-%d %H:%M:%S.%f")
    # return format based on rollup or default of "s"
    return(formats[[resolution %||% "s"]])
}

#' Check that a value is TRUE or FALSE
#'
#' @param value Value to check
#'
#' @return `TRUE` if `value` is either `TRUE` or `FALSE`, `FALSE` otherwise
#'
#' @keywords internal
is.TRUEorFALSE <- function (value) {
    return(is.logical(value) && !is.na(value) && length(value) == 1)
}

escapeQuotes <- function(str) {
    gsub("'", "\\\\'", str)
}

#' Check if a user has packages installed
#'
#' @param pkgs a character vector of package names to check.
#'
#' @return nothing, called for side effects
#'
#' @keywords internal
checkInstalledPackages <- function (pkgs) {
    installed <- pkgs %in% rownames(installed.packages())
    if (!all(installed)){
        halt("Missing required packages: ", serialPaste(dQuote(pkgs[!installed])))
    }
}

#' Check if a package has a function installed
#'
#' This is useful for handling if the user has the correct version of a suggested
#' package installed.
#'
#' @param fun
#' @param pkg
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
escapeRegex <- function (string) {
    out <- gsub("([.|()\\^{}+$*?])", "\\\\\\1", string)
    return(gsub("(\\[|\\])", "\\\\\\1", out))
}
