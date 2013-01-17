##' Main Crunch API handling function
##' @param http.verb character in GET, PUT, POST
##' @param url character URL to do the verb on
##' @param ... additional arguments
##' @param auth.required logical: does this API call require authentication? If 
##' so, the function will try to fetch the session token from the local store,
##' and it will throw an error if no session has been authenticated
crunchAPI <- function (http.verb, url, response.handler=handleAPIresponse, config=list(verbose=FALSE), ...) {
    configs <- update.list(crunchConfig(), config)
    x <- selectHttpFunction(http.verb)(url, ..., config=configs)
    out <- response.handler(x)
    return(out)
}

##' @importFrom httr content stop_for_status http_status
handleAPIresponse <- function (response) {
    if (http_status(response)$category == "success") {
        return(content(response))
    } else {
        stop_for_status(response)
    }
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
##' @importFrom httr GET PUT POST
##' @param x character HTTP verb name
##' @return the corresponding function from the \code{httr} package
selectHttpFunction <- function (x) {
    x <- list(GET=GET, PUT=PUT, POST=POST)[[toupper(x)]]
    stopifnot(is.function(x))
    return(x)
}

getAPIroot <- function () {
    crunchAPI("GET", getOption("crunch.api.endpoint"))
}

