##' Stay authenticated.
##'
##' The auth store keeps the session token after authentication so that all 
##' API calls can use it.
##'
##' @format An environment.
##' @keywords internal
session_store <- NULL
makeSessionStore <- function () {
    session_store <<- new.env(hash = TRUE, parent = emptyenv())
}
makeSessionStore()

##' Kill the active Crunch session
##' @export 
logout <- function () {
    logging_out <- sessionURL("logout_url")
    if (!is.null(logging_out)) GET(logging_out)
    deleteSessionInfo()
}

deleteSessionInfo <- function () {
    rm(list=ls(envir=session_store), envir=session_store)
}

##' Authenticate with the Crunch API
##' @param email the user's email address
##' @param ... additional parameters
##' @export 
login <- function (email, ...) {
    out <- crunchAuth(email, ...)
    saveToken(out$cookies)
    saveSessionURLs(getAPIroot()$urls)
    saveSessionURLs(getUserURLs())
    saveDatasetURLs()    
    invisible()
}

crunchAuth <- function (email, ...) {
    POST(getOption("crunch.api.endpoint"), body=basicAuthArgs(email))
}

##' @importFrom RJSONIO toJSON
basicAuthArgs <- function (x) {
    return(toJSON(list(email=x)))
}


##' @importFrom httr set_cookies
saveToken <- function (cookie) {
    session_store$cookie <- do.call("set_cookies", cookie)
}

saveSessionURLs <- function (x) {
    if (is.null(session_store$urls)) session_store$urls <- list()
    session_store$urls <- update(session_store$urls, x)
}

sessionURL <- function (x=NULL) {
    out <- session_store$urls
    if (!is.null(x)) out <- out[[x]]
    return(out)
}

getToken <- function () session_store$cookie