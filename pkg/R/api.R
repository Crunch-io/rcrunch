##' Main Crunch API handling function
##' @param http.verb character in GET, PUT, POST
##' @param url character URL to do the verb on
##' @param ... additional arguments
##' @param response.handler function that takes a http response object and does something with it
##' @param config list of config parameters. See httr documentation.
crunchAPI <- function (http.verb, url, response.handler=handleAPIresponse, config=list(verbose=FALSE), status.handlers=list(), ...) {
    configs <- updateList(crunchConfig(), config)
    url ## force lazy eval of url before inserting in try() below
    x <- try(selectHttpFunction(http.verb)(url, ..., config=configs), 
        silent=TRUE)
    if (length(status.handlers)) {
        out <- response.handler(x, special.statuses=status.handlers)
    } else {
        out <- response.handler(x)
    }
    return(out)
}

GET <- function (...) {
    crunchAPI("GET", ...)
}

PUT <- function (...) {
    crunchAPI("PUT", ...)
}

POST <- function (...) {
    crunchAPI("POST", ...)
}

##' Do the right thing with the HTTP response
##' @param response an httr response object
##' @param special.statuses an optional named list of functions by status code.
##' @return The full HTTP response object, just the content, or any other status-specific action 
##' @importFrom httr content stop_for_status http_status
handleAPIresponse <- function (response, special.statuses=list()) {
    response <- handleAPIerror(response)
    code <- response$status_code
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
                class(out) <- "shoji"
            }
            return(out)            
        }
    } else {
        stop_for_status(response)
    }
}

handleAPIerror <- function (response) {
    if (is.error(response)) {
        if (crunchIsDown(response)) {
            stop("Cannot connect to Crunch API", call.=FALSE)
        } else {
            stop(attr(response, "condition"))
        }
    }
    return(response)    
}

crunchConfig <- function () {
    tk <- getToken()
    if (is.null(tk)) tk <- list()
    c(tk, crunchHTTPheaders())
}

crunchHTTPheaders <- function () {
    list(httpheader=list(`User-Agent`=crunchUserAgent()))
}

crunchUserAgent <- function () {
    paste("rcrunch", packageVersion("rcrunch"))
}

simpleResponseStatus <- function (code) {
    as.integer(code) %/% 100L
}

is.JSON.response <- function (x) {
    isTRUE(grepl("application/json", x$headers[["content-type"]], fixed=TRUE))
}

##' @importFrom RJSONIO fromJSON
parseJSONresponse <- function (x, simplifyWithNames=FALSE, ...) {
    RJSONIO:::fromJSON(httr:::parse_text(x, encoding = "UTF-8"),
        simplifyWithNames=simplifyWithNames, ...)
}

##' Select the right function to run that HTTP call
##' @param x character HTTP verb name
##' @return the corresponding function from the \code{httr} package
selectHttpFunction <- function (x) {
    x <- list(GET=httr:::GET, PUT=httr:::PUT, POST=httr:::POST)[[toupper(x)]]
    stopifnot(is.function(x))
    return(x)
}

getAPIroot <- function () {
    GET(getOption("crunch.api.endpoint"))
}

crunchAPIcanBeReached <- function () {
    testing <- try(getAPIroot(), silent=TRUE)
    return(!crunchIsDown(testing))
}

crunchIsDown <- function (response) {
    is.error(response) && "COULDNT_CONNECT" %in% class(attr(response, "condition"))
}
