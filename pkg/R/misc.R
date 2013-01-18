is.error <- function (x) inherits(x, "try-error")

update.list <- function (x, y) {
    x[names(y)] <- y
    return(x)
}

##' Generic List Element Extractor
##'
##'
##' @param key character naming the key(s) to extract
##' @param xlist list containing other lists from which you want to extract
##' @param ifnot what to return if the key is not found in a given xlist element
##' @param simplify logical, passed to sapply internally
##' @return the requested element(s). If length(key)>1, a named list of those
##' elements
##' @export
selectFrom <- function (key, xlist, ifnot=NA, simplify=TRUE) {
    #stopifnot(is.list(xlist))
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
##' f <- fetchQDF("econ0003x20120623")
##' selectFromWhere(name=="birthyr", f$questions)
##' @export
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