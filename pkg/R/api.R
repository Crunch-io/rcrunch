##' Main Crunch API handling function
##' @param http.verb character in GET, PUT, POST
##' @param url character URL to do the verb on
##' @param ... additional arguments
##' @param auth.required logical: does this API call require authentication? If 
##' so, the function will try to fetch the session token from the local store,
##' and it will throw an error if no session has been authenticated
crunchAPI <- function (http.verb, url, ..., auth.required=TRUE) {
    auth <- getToken()
    if (is.null(auth) && auth.required) {
        stop("Please authenticate first")
    }
    http.verb <- validateHttpVerb(http.verb)
    http.verb <- selectHttpFunction(http.verb)
    
    out <- http.verb(url, ..., config=list(auth))
    
    if (length(out$content)) {
        if (is.JSON.response(out)) {
            out <- parseJSONresponse(out$content)
        }
    }
    return(out)
}

is.JSON.response <- function (x) {
    isTRUE(grepl("application/json", x$headers[["content-type"]], fixed=TRUE))
}

##' @importFrom RJSONIO fromJSON
parseJSONresponse <- function (x) {
    fromJSON(rawToChar(x), simplifyWithNames=FALSE)
}

supported.verbs <- c("GET", "PUT", "POST")

##' Validate HTTP verbs
##' @param x character
##' @param validated x
validateHttpVerb <- function (x) {
    if (!is.character(x) || length(x) != 1) {
        stop("Must supply exactly one HTTP verb (character)")
    }
    x <- intersect(x, supported.verbs)
    if (length(x)==0) {
        stop("Unsupported verb")
    }
    return(x)
}

##' Select the right function to run that HTTP call
##' @importFrom httr GET PUT POST
##' @param x character HTTP verb name
##' @return the corresponding function from the \code{httr} package
selectHttpFunction <- function (x) {
    x <- list(GET=GET, PUT=PUT, POST=POST)[[x]]
    stopifnot(is.function(x))
    return(x)
}

getAPIroot <- function () {
    crunchAPI("GET", .crunch_api())
}

