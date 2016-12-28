is.error <- function (x) inherits(x, "try-error")

rethrow <- function (x) halt(errorMessage(x))

errorMessage <- function (e) attr(e, "condition")$message

vget <- function (name) {
    ## Return a function you can lapply/vapply to select an attribute
    ## Usage: lapply(list.of.stuff, vget("name"))
    ## instead of: lapply(list.of.stuff, function (x) x$name)
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

askForPermission <- function (prompt="") {
    ## If options explicitly say we don't need to ask, bail.
    ## Have to check that it's FALSE and not NULL. Silence doesn't mean consent.
    must.confirm <- getOption("crunch.require.confirmation") %||% TRUE
    if (must.confirm == FALSE) return(TRUE)

    ## If we're here but not interactive, we can't give permission.
    if (!interactive()) return(FALSE)
    prompt <- paste(prompt, "(y/n) ")
    proceed <- ""
    while (!(proceed %in% c("y", "n"))) {
        proceed <- tolower(readline(prompt))
    }
    return(proceed == "y")
}

emptyObject <- function (...) {
    ## toJSON(list()) is "[]". toJSON(emptyObject()) is "{}"
    ##
    ## Make the function take ... so you can *apply over something and just
    ## call the function
    structure(list(), .Names=character(0))
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
