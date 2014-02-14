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
##' @importFrom httr content stop_for_status http_status
handleAPIresponse <- function (response, special.statuses=list()) {
    response <- handleAPIerror(response)
    code <- response$status_code
    if (isTRUE(getOption("crunch.debug"))) message(code)
    handler <- special.statuses[[as.character(code)]]
    if (is.function(handler)) {
        invisible(handler(response))
    } else if (http_status(response)$category == "success") {
        if (code==201) {
            return(response$headers$location)
        } else if (code==204 || length(response$content)==0) {
            invisible(response)
        } else {
            out <- content(response)
            if (is.shoji.like(out)) {
                class(out) <- c("shoji", out$element)
            }
            if ("shoji:view" %in% class(out)) {
                out <- out$value
            }
            return(out)            
        }
    } else {
        if (isTRUE(getOption("crunch.debug"))) {
            out <- try(content(response), silent=TRUE)
            if (!inherits(out, "try-error") && "message" %in% try(names(out))) {
                message(response$message)
            }            
        }
        stop_for_status(response)
    }
}

handleAPIerror <- function (response) {
    if (is.error(response)) {
        if (attr(response, "condition")$message == "Empty reply from server"){
            stop("Server did not respond. Please check your local configuration and try again later.", call.=FALSE)
        } else if (crunchIsDown(response)) {
            stop("Cannot connect to Crunch API", call.=FALSE)
        } else {
            stop(attr(response, "condition"))
        }
    }
    return(response)    
}

crunchConfig <- function () {
    c(getToken(), list(verbose=FALSE, sslversion=3L), crunchHTTPheaders())
}

crunchHTTPheaders <- function () {
    list(httpheader=list(`User-Agent`=getCrunchUserAgent()))
}

crunchUserAgent <- function () {
    rc <- paste("rcrunch", packageVersion("rcrunch"))
    try(rc <- paste(httr:::default_ua(), rc, sep=" / "), silent=TRUE)
    return(rc)
}

setCrunchUserAgent <- function () {
    session_store$user_agent <- crunchUserAgent()
}

getCrunchUserAgent <- function () {
    ua <- session_store$user_agent
    if (is.null(ua)) { ## Should only happen if not logged in
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

##' Select the right function to run that HTTP call
##' @param x character HTTP verb name
##' @return the corresponding function from the \code{httr} package
selectHttpFunction <- function (x) {
    x <- list(GET=httr:::GET, PUT=httr:::PUT, POST=httr:::POST,
        DELETE=httr:::DELETE)[[toupper(x)]]
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
