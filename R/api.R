##' Main Crunch API handling function
##' @param http.verb character in GET, PUT, POST, PATCH, DELETE
##' @param url character URL to do the verb on
##' @param ... additional arguments passed to \code{GET}, \code{PUT},
##' \code{POST}, \code{PATCH}, or \code{DELETE}
##' @param config list of config parameters. See httr documentation.
##' @param status.handlers named list of specific HTTP statuses and a response
##' function to call in the case where that status is returned. Passed to the
##' \code{\link{handleAPIresponse}} function.
##' @keywords internal
crunchAPI <- function (http.verb, url, config=list(), status.handlers=list(), ...) {
    url ## force lazy eval of url before inserting in try() below
    if (isTRUE(getOption("crunch.debug"))) {
        ## TODO: work this into crunch.log
        try(cat("\n", list(...)$body, "\n"), silent=TRUE)
    }
    FUN <- get(paste0("c", http.verb), envir=asNamespace("crunch"))
    x <- try(FUN(url, ..., config=config), silent=TRUE)
    out <- handleAPIresponse(x, special.statuses=status.handlers)
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

##' Do the right thing with the HTTP response
##' @param response an httr response object
##' @param special.statuses an optional named list of functions by status code.
##' @return The full HTTP response object, just the content, or any other
##' status-specific action
##' @importFrom httr content http_status
##' @keywords internal
handleAPIresponse <- function (response, special.statuses=list()) {
    response <- handleAPIerror(response)
    logMessage(responseStatusLog(response))
    code <- response$status_code
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
        if (code == 401) {
            halt("You are not authenticated. Please `login()` and try again.")
        } else if (code == 410) {
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
        if (code == 409 && grepl("current editor", msg)) {
            halt("You are not the current editor of this dataset. `unlock()` it and try again.")
        }
        halt(msg)
    }
}

handleAPIerror <- function (response) {
    if (is.error(response)) {
        if (attr(response, "condition")$message == "Empty reply from server"){
            halt("Server did not respond. Please check your local ",
                "configuration and try again later.")
        } else {
            rethrow(response)
        }
    }
    return(response)    
}

responseStatusLog <- function (response) {
    req <- response$request
    return(paste("HTTP",
        req$method, 
        req$url, 
        response$status_code,
        # req$headers[["content-length"]] %||% 0,
        # response$headers[["content-length"]] %||% 0,
        response$times["total"]))
}

##' @importFrom httr config add_headers
crunchConfig <- function () {
    return(c(config(verbose=isTRUE(getOption("crunch.debug")), postredir=3),
        add_headers(`user-agent`=crunchUserAgent())))
}

##' @importFrom utils packageVersion
##' @importFrom curl curl_version
crunchUserAgent <- function (x) {
    ## Cf. httr:::default_ua
    versions <- c(
        libcurl = curl_version()$version,
        curl = as.character(packageVersion("curl")),
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
