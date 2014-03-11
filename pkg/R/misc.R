is.error <- function (x) inherits(x, "try-error")

updateList <- function (x, y) {
    x[names(y)] <- y
    return(x)
}

##' Generic List Element Extractor
##'
##'
##' @param key character naming the key(s) to extract. Can traverse list elements by separating them with \code{$}.
##' @param xlist list containing other lists from which you want to extract
##' @param ifnot what to return if the key is not found in a given xlist element
##' @param simplify logical, passed to sapply internally
##' @return the requested element(s). If length(key)>1, a named list of those
##' elements
selectFrom <- function (key, xlist, ifnot=NA, simplify=TRUE) {
    if (!is.list(xlist)) {
        stop("xlist must be a list object")
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
##' @examples
##' vars <- loadJSONMocks("variables.json")
##' selectFromWhere(name=="Gender", vars)
selectFromWhere <- function (where=TRUE, xlist, key=NULL, ifnot=NA,
                            simplify=TRUE) {
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

##' Collapse string vector for making docs
##'
##' Empty strings will be replaced with newlines. 
##' @param x character
##' @return character vector of length 1
doc <- function (x) {
    x[nchar(x)==0] <- "\n"
    return(paste0("\n ", paste(x, collapse=" "), "\n"))
}

##' Make a prose list
##' Function to paste together a list of items, separated by commas (if more than 2), and with the last one having the collapse string.
##'
##' @param x vector or list
##' @param collapse default="and"
serialPaste <- function (x, collapse="and") {
	if (length(x)>1) x[length(x)] <- paste(collapse, x[length(x)])
	join.with <- ifelse(length(x)>2, ", ", " ")
	return(paste(x, collapse=join.with))
}

##' Load API sample objects for testing
##' @param filename character the name of the file. Current options are 
##' "dataset.json", "variables.json", "summaries.json".
##' @return The deserialized contents of the JSON file, typically a list.
##' @importFrom RJSONIO fromJSON
loadJSONMocks <- function (filename) {
    fromJSON(system.file(filename, package="rcrunch", mustWork=TRUE),
        simplifyWithNames=FALSE)
}

loadMockVariables <- function () {
    files <- dir(paste0(system.file("api/datasets/dataset1/variables/", package="rcrunch", mustWork=TRUE), "*.json"), 
        full.names=TRUE)
    out <- lapply(files, function (x) {
        structure(fromJSON(x, simplifyWithNames=FALSE), class="shoji")
    })
    names(out) <- sub("\\.json", "", basename(files))
    return(out)
}