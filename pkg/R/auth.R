##' Stay authenticated.
##'
##' The auth store keeps the session token after authentication so that all 
##' API calls can use it.
##'
##' @format An environment.
##' @keywords internal
session_store <- NULL
makeAuthStore <- function () {
    session_store <<- new.env(hash = TRUE, parent = emptyenv())
}
makeAuthStore()

##' Kill the active Crunch session
##' @export 
logout <- function () {
    rm(list=ls(envir=session_store), envir=session_store)
}

##' Authenticate with the Crunch API
##' @param email the user's email address
##' @param ... additional parameters
##' @export 
login <- function (email, ...) {
    out <- crunchAPI("POST", .crunch_api(),
        body=basicAuthArgs(email), auth.required=FALSE)
    
    saveToken(out$cookies)
    invisible()
}

##' @importFrom RJSONIO toJSON
basicAuthArgs <- function (x) {
    return(toJSON(list(email=x)))
}


##' @importFrom httr set_cookies
saveToken <- function (cookie) {
    session_store$cookie <- do.call("set_cookies", cookie)
}

getToken <- function () session_store$cookie