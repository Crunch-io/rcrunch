##' Main Crunch API handling function
##' @param http.verb character in GET, PUT, POST
##' @param url character URL to do the verb on
##' @param ... additional arguments passed to \code{GET}, \code{PUT}, or
##' \code{POST}
##' @param response.handler function that takes a http response object and does
##' something with it
##' @param config list of config parameters. See httr documentation.
##' @param status.handlers named list of specific HTTP statuses and a response
##' function to call in the case where that status is returned. Passed to the
##' \code{response.handler} function.
crunchAPI <- function (http.verb, url, response.handler=handleAPIresponse, config=list(), status.handlers=list(), ...) {
    configs <- updateList(crunchConfig(), config)
    url ## force lazy eval of url before inserting in try() below
    if (isTRUE(getOption("crunch.debug"))) message(paste(http.verb, url))
    x <- try(selectHttpFunction(http.verb)(url, ..., config=configs), 
        silent=TRUE)
    if (length(status.handlers)) {
        out <- response.handler(x, special.statuses=status.handlers)
    } else {
        out <- response.handler(x)
    }
    return(out)
}

## So that we can swap them out for testing
http_verbs <- NULL
makeHTTPStore <- function () {
    http_verbs <<- new.env(hash = TRUE, parent = emptyenv())
}
addRealHTTPVerbs <- function () {
    http_verbs$GET <- function (...) crunchAPI("GET", ...)
    http_verbs$PUT <- function (...) crunchAPI("PUT", ...)
    http_verbs$PATCH <- function (...) crunchAPI("PATCH", ...)
    http_verbs$POST <- function (...) crunchAPI("POST", ...)
    http_verbs$DELETE <- function (...) crunchAPI("DELETE", ...)
}
makeHTTPStore()
addRealHTTPVerbs()

GET <- function (...) {
    http_verbs$GET(...)
}

PUT <- function (...) {
    http_verbs$PUT(...)
}

PATCH <- function (...) {
    http_verbs$PATCH(...)
}

POST <- function (...) {
    http_verbs$POST(...)
}

DELETE <- function (...) {
    http_verbs$DELETE(...)
}

##' Do the right thing with the HTTP response
##' @param response an httr response object
##' @param special.statuses an optional named list of functions by status code.
##' @return The full HTTP response object, just the content, or any other
##' status-specific action 
##' @importFrom httr content http_status
handleAPIresponse <- function (response, special.statuses=list()) {
    response <- handleAPIerror(response)
    code <- response$status_code
    if (isTRUE(getOption("crunch.debug"))) message(code)
    handler <- special.statuses[[as.character(code)]]
    if (is.function(handler)) {
        invisible(handler(response))
    } else if (http_status(response)$category == "success") {
        if (code == 201) {
            return(response$headers$location)
        } else if (code == 204 || length(response$content) == 0) {
            invisible(response)
        } else {
            return(handleShoji(content(response)))            
        }
    } else {
        if (code == 410) {
            stop("The API resource at ",
                response$url, 
                " has moved permanently. Please upgrade rcrunch to the ",
                "latest version.", call.=FALSE)
        }
        msg <- http_status(response)$message
        msg2 <- try(content(response)$message, silent=TRUE)
        if (!is.error(msg2)) {
            msg <- paste(msg, msg2, sep=": ")
        }
        stop(msg, call.=FALSE)
    }
}

handleAPIerror <- function (response) {
    if (is.error(response)) {
        if (attr(response, "condition")$message == "Empty reply from server"){
            stop("Server did not respond. Please check your local configuration and try again later.", call.=FALSE)
        } else if (crunchIsDown(response)) {
            stop("Cannot connect to Crunch API", call.=FALSE)
        } else {
            rethrow(response)
        }
    }
    return(response)    
}

##' @importFrom httr add_headers
crunchConfig <- function () {
    c(getToken(), httr:::default_config(), list(verbose=isTRUE(getOption("crunch.debug")), sslversion=3L), add_headers(`user-agent`=getCrunchUserAgent()))
}

crunchUserAgent <- function (x) {
    rc <- paste("rcrunch", packageVersion("rcrunch"), sep="/")
    try(rc <- paste(httr:::default_ua(), rc), silent=TRUE)
    if (!missing(x)) rc <- paste(rc, x)
    return(rc)
}

setCrunchUserAgent <- function (x) {
    session_store$.globals$user_agent <- crunchUserAgent(x)
}

getCrunchUserAgent <- function () {
    ua <- session_store$.globals$user_agent
    if (is.null(ua)) { ## Should not happen
        ua <- crunchUserAgent()
    }
    return(ua)
}

simpleResponseStatus <- function (code) {
    as.integer(code) %/% 100L
}

is.JSON.response <- function (x) {
    isTRUE(grepl("application/json", x$headers[["content-type"]], fixed=TRUE))
}

##' @importFrom RJSONIO fromJSON
## Looks like httr has switched to RJSONIO, so this may not be necessary any more
parseJSONresponse <- function (x, simplifyWithNames=FALSE, ...) {
    fromJSON(httr:::parse_text(x, encoding = "UTF-8"),
        simplifyWithNames=simplifyWithNames, ...)
}

handleShoji <- function (x) {
    if (is.shoji.like(x)) {
        class(x) <- c("shoji", x$element)
    }
    if ("shoji:view" %in% class(x)) {
        x <- x$value
    }
    return(x)
}

##' Select the right function to run that HTTP call
##' @param x character HTTP verb name
##' @return the corresponding function from the \code{httr} package
selectHttpFunction <- function (x) {
    x <- list(GET=httr:::GET, PUT=httr:::PUT, POST=httr:::POST,
        DELETE=httr:::DELETE, PATCH=httr:::PATCH)[[toupper(x)]]
    stopifnot(is.function(x))
    return(x)
}

getAPIroot <- function () {
    GET(getOption("crunch.api"))
}

crunchAPIcanBeReached <- function () {
    testing <- try(getAPIroot(), silent=TRUE)
    return(!crunchIsDown(testing))
}

crunchIsDown <- function (response) {
    is.error(response) && "COULDNT_CONNECT" %in% class(attr(response, "condition"))
}
