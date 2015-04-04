cache <- NULL
initCache <- function () {
    cache <<- new.env(hash=TRUE)
}
initCache()

caching <- function () isTRUE(cache[["on"]])

cacheOn <- function () cache$on <- TRUE
cacheOff <- function () rm(list=ls(all=TRUE), envir=cache)
clearCache <- function () {
    cacheOff()
    cacheOn()
}

## deal with query params? 
dropCache <- function (x) {
    ## Drop x and anything below it in the tree
    dropPattern(paste0("^", regexEscape(x)))
}
dropOnly <- function (x) {
    log("DROP", x)
    suppressWarnings(rm(list=x, envir=cache))
}
dropBelow <- function (x) {
    ## Don't drop x, just those below it in the tree. hence ".+"
    dropPattern(paste0("^", regexEscape(x), ".+")) 
}
dropPattern <- function (x, escape=TRUE) {
    log(paste0("DROP", x))
    rm(list=ls(envir=cache, pattern=x), envir=cache)
}

## TODO: write this
regexEscape <- function (x) {
    ## Escape all reserved characters with \\
    return(x)
}

##' @importFrom httr GET
cGET <- function (url, ...) {
    # Always check cache. Just don't write to cache if cache is off
    
    Call <- match.call(expand.dots = TRUE)
    cache.url <- url
    if (!is.null(Call[["query"]])) {
        cache.url <- paste0(url, "?", toQuery(eval.parent(Call$query)))
    }
    if (exists(cache.url, envir=cache)) {
        log("HIT", cache.url)
        return(get(cache.url, envir=cache))
    }
    x <- GET(url, ...)
    if (caching() && x$status_code == 200) {
        log("SET", cache.url)
        assign(cache.url, x, envir=cache)
    }
    return(x)
}

##' @importFrom httr PUT
cPUT <- function (url, ..., drop=dropCache(url)) {
    x <- PUT(url, ...)
    force(drop)
    return(x)
}

##' @importFrom httr POST
cPOST <- function (url, ..., drop=dropOnly(url)) {
    x <- POST(url, ...)
    force(drop)
    return(x)
}

##' @importFrom httr PATCH
cPATCH <- function (url, ..., drop=dropCache(url)) {
    x <- PATCH(url, ...)
    force(drop)
    return(x)
}

##' @importFrom httr DELETE
cDELETE <- function (url, ..., drop=dropCache(url)) {
    x <- DELETE(url, ...)
    force(drop)
    return(x)
}

##' @importFrom RCurl curlEscape
toQuery <- function (query) {
    if (is.list(query)) {
        names <- curlEscape(names(query))
        values <- curlEscape(query)
        query <- paste0(names, "=", values, collapse = "&")
    }
    return(query)
}

