is.error <- function (x) inherits(x, "try-error")

halt <- function (...) {
    log("ERROR", ..1)
    stop(..., call.=FALSE)
}

rethrow <- function (x) halt(attr(x, "condition")$message)

updateList <- function (x, y) {
    x[names(y)] <- y
    return(x)
}

selectFrom <- function (key, xlist, ifnot=NA, simplify=TRUE) {
    ##' Generic List Element Extractor
    ##'
    ##' @param key character naming the key(s) to extract. Can traverse list
    ##' elements by separating them with \code{$}.
    ##' @param xlist list containing other lists from which you want to extract
    ##' @param ifnot what to return if the key is not found in a given xlist element
    ##' @param simplify logical, passed to sapply internally
    ##' @return the requested element(s). If length(key)>1, a named list of those
    ##' elements
    if (!is.list(xlist)) {
        halt("xlist must be a list object")
    }
    if (length(key)>1) {
        y <- sapply(key, selectFrom, xlist, ifnot, simplify=FALSE)
    } else {
    	y <- sapply(xlist,
    	    function (x) {
    	        key <- unlist(strsplit(key, "$", fixed=TRUE))
    	        for (i in key) {
    	            if (!is.list(x)) x <- NULL
                    if (!is.null(x)) x <- x[[i]]
                }
                if (is.null(x)) x <- ifnot
                return(x)
    	    }, simplify=simplify)
    }
    return(y)
}

selectFromWhere <- function (where=TRUE, xlist, key=NULL, ifnot=NA,
                            simplify=TRUE) {
    ##' Extract list element(s) conditionally
    ##'
    ##' @param where an expression to be evaluated in each element in \code{xlist}.
    ##' This determines which list objects to keep. The expression must return a
    ##' logical.
    ##' @param key character naming the key(s) to extract. Default is \code{NULL},
    ##' meaning that the entire list element is to be kept.
    ##' @param xlist list containing other lists from which you want to extract.
    ##' Passed to \code{\link{selectFrom}}.
    ##' @param ifnot what to return if the key is not found in a given xlist
    ##' element. Passed to \code{\link{selectFrom}}.
    ##' @param simplify logical, passed to \code{\link{selectFrom}}.
    where <- substitute(where)
    selectthese <- vapply(xlist, function (x) try(eval(where, x)), logical(1))

    if (!any(selectthese)) return(list())
    xlist <- xlist[selectthese]
    if (simplify && is.null(key) && length(xlist)==1) {
        return(xlist[[1]])
    } else if (!is.null(key)) {
        return(selectFrom(key, xlist, ifnot, simplify))
    } else {
        return(xlist)
    }
}

serialPaste <- function (x, collapse="and") {
    ##' Make a prose list
    ##' Function to paste together a list of items, separated by commas (if more
    ##' than 2), and with the last one having the collapse string.
    ##'
    ##' @param x vector or list
    ##' @param collapse default="and"
	if (length(x)>1) x[length(x)] <- paste(collapse, x[length(x)])
	join.with <- ifelse(length(x)>2, ", ", " ")
	return(paste(x, collapse=join.with))
}

now <- function () strftime(Sys.time(), usetz=TRUE)

##' @importFrom httr parse_url build_url
absoluteURL <- function (urls, base) {
    ## Detect if we have relative urls, and then concatenate if so
    if (length(urls) && ## if there is anything to munge
        !any(substr(urls, 1, 4) == "http")) { ## the urls don't start with http
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
        }
    return(urls)
}

joinPath <- function (base.path, relative.part) {
    first.char <- substr(relative.part, 1, 1)
    if (first.char == "/") {
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
    last.char <- substr(relative.part, nchar(relative.part),
        nchar(relative.part))
    if (last.char == "/") {
        out <- paste0(out, "/")
    }
    return(out)
}

## Borrowed from Hadley
"%||%" <- function (a, b) if (!is.null(a)) a else b

