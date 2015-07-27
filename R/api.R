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
##' @keywords internal
crunchAPI <- function (http.verb, url, response.handler=handleAPIresponse, config=list(), status.handlers=list(), ...) {
    url ## force lazy eval of url before inserting in try() below
    if (isTRUE(getOption("crunch.debug"))) message(paste(http.verb, url))
    FUN <- get(paste0("c", http.verb), envir=asNamespace("crunch"))
    # FUN <- get(http.verb, envir=asNamespace("httr"))
    x <- try(FUN(url, ..., config=config), silent=TRUE)
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

##' HTTP methods for communicating with the Crunch API
##'
##' @param ... see \code{\link{crunchAPI}} for details. \code{url} is the first
##' named argument and is required; \code{body} is also required for PUT, 
##' PATCH, and POST. 
##' @return Depends on the response status of the HTTP request and any custom
##' handlers.
##' @name http-methods
##' @export
crGET <- function (...) http_verbs$GET(...)
##' @rdname http-methods
##' @export
crPUT <- function (...) http_verbs$PUT(...)
##' @rdname http-methods
##' @export
crPATCH <- function (...) http_verbs$PATCH(...)
##' @rdname http-methods
##' @export
crPOST <- function (...) http_verbs$POST(...)
##' @rdname http-methods
##' @export
crDELETE <- function (...) http_verbs$DELETE(...)

##' @importFrom httr content http_status
handleAPIresponse <- function (response, special.statuses=list()) {
    ##' Do the right thing with the HTTP response
    ##' @param response an httr response object
    ##' @param special.statuses an optional named list of functions by status code.
    ##' @return The full HTTP response object, just the content, or any other
    ##' status-specific action 
    response <- handleAPIerror(response)
    code <- response$status_code
    if (isTRUE(getOption("crunch.debug"))) message(code)
    handler <- special.statuses[[as.character(code)]]
    if (is.function(handler)) {
        invisible(handler(response))
    } else if (http_status(response)$category == "success") {
        if (code %in% c(201, 202) && length(response$headers$location)) {
            return(response$headers$location)
        } else if (code == 204 || length(response$content) == 0) {
            invisible(response)
        } else {
            return(handleShoji(content(response)))            
        }
    } else {
        if (code == 410) {
            halt("The API resource at ",
                response$url, 
                " has moved permanently. Please upgrade crunch to the ",
                "latest version.")
        }
        msg <- http_status(response)$message
        msg2 <- try(content(response)$message, silent=TRUE)
        if (!is.error(msg2)) {
            msg <- paste(msg, msg2, sep=": ")
        }
        halt(msg)
    }
}

handleAPIerror <- function (response) {
    if (is.error(response)) {
        if (attr(response, "condition")$message == "Empty reply from server"){
            halt("Server did not respond. Please check your local ",
                "configuration and try again later.")
        } else if (crunchIsDown(response)) {
            halt("Cannot connect to Crunch API")
        } else {
            rethrow(response)
        }
    }
    return(response)    
}

##' @importFrom httr config add_headers
crunchConfig <- function () {
    new.httr <- unlist(packageVersion("httr"))[1] == 1
    if (new.httr) {
        return(c(config(verbose=isTRUE(getOption("crunch.debug")),
                        postredir=3),
            add_headers(`user-agent`=crunchUserAgent())))
    } else {
        return(config(verbose=isTRUE(getOption("crunch.debug")),
            sslversion="SSLVERSION_TLSv1_2",
            encoding="gzip", ## In httr default config, but to be sure
            httpheader=c(`user-agent`=crunchUserAgent(),
                         `accept-encoding`="gzip")))
    }
}

##' @importFrom utils packageVersion
##' @importFrom RCurl curlVersion
crunchUserAgent <- function (x) {
    ## Cf. httr:::default_ua
    versions <- c(
        curl = RCurl::curlVersion()$version,
        Rcurl = as.character(packageVersion("RCurl")),
        httr = as.character(packageVersion("httr")),
        rcrunch = as.character(packageVersion("crunch"))
    )
    ua <- paste0(names(versions), "/", versions, collapse = " ")
    if (!missing(x)) ua <- paste(ua, x)
    return(ua)
}

handleShoji <- function (x) {
    if (is.shoji.like(x)) {
        class(x) <- c("shoji", x$element)
    }
    if ("shoji:view" %in% class(x) && !is.shoji.order.like(x)) {
        x <- x$value
    }
    return(x)
}

getAPIroot <- function (x=getOption("crunch.api")) {
    ShojiObject(crGET(x))
}

sessionURL <- function (key, collection="catalogs") {
    if (is.authenticated()) {
        return(shojiURL(session_store$root, collection, key))
    } else {
        halt("You must authenticate before making this request")
    }
}

rootURL <- function (x, obj=session_store$root) {
    ## DEPRECATE ME
    if (!is.authenticated()) {
        halt("You must authenticate before making this request")
    }
    if (is.shojiObject(obj)) {
        return(obj@urls[[paste0(x, "_url")]])
    } else {
        return(NULL)
    }
}

crunchAPIcanBeReached <- function () {
    testing <- try(getAPIroot(), silent=TRUE)
    return(!crunchIsDown(testing))
}

crunchIsDown <- function (response) {
    is.error(response) && "COULDNT_CONNECT" %in% class(attr(response, "condition"))
}
