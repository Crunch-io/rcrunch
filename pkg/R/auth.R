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
    session_store$.globals <- list(prompt=getOption("prompt"))
}
makeSessionStore()

##' Kill the active Crunch session
##' @export 
logout <- function () {
    if (is.authenticated()) try(GET(sessionURL("logout_url")), silent=TRUE) ## hack?
    deleteSessionInfo()
    options(prompt = session_store$.globals$prompt)
}

deleteSessionInfo <- function () {
    rm(list=setdiff(ls(envir=session_store), ".globals"), envir=session_store)
}

##' Authenticate with the Crunch API
##' @param email the user's email address
##' @param ... additional parameters
##' @export 
login <- function (email, ...) {
    logout()
    out <- crunchAuth(email, ...)
    saveUser(email)
    saveToken(out$cookies)
    saveSessionURLs(getAPIroot()$urls)
    saveSessionURLs(getUserURLs())
    updateDatasetList()
    message("Logged into crunch.io as ", email)
    options(prompt = paste("[crunch]", session_store$.globals$prompt)) 
    invisible()
}

saveUser <- function (x) {
    session_store$email <- x
}

crunchAuth <- function (email, ...) {
    POST(getOption("crunch.api.endpoint"), body=basicAuthArgs(email, ...), 
        status.handlers=list(`401`=function (response, user=email) {
            stop(paste("Unable to authenticate", user), call.=FALSE)
        }))
}

##' @importFrom RJSONIO toJSON
basicAuthArgs <- function (x, ...) {
    return(toJSON(list(email=x, ...)))
}

##' @importFrom httr set_cookies
saveToken <- function (cookie) {
    session_store$cookie <- do.call("set_cookies", cookie)
}

saveSessionURLs <- function (x) {
    if (is.null(session_store$urls)) session_store$urls <- list()
    session_store$urls <- updateList(session_store$urls, x)
}

sessionURL <- function (x=NULL) {
    if (is.authenticated()) {
        out <- session_store$urls
        if (!is.null(x)) out <- out[[x]]
        return(out)
    } else {
        stop("You must authenticate before making this request")
    }
}

getToken <- function () session_store$cookie

is.authenticated <- function () !is.null(getToken())