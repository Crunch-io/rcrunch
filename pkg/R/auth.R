##' Stay authenticated.
##'
##' The auth store keeps the session token after authentication so that all 
##' API calls can use it.
##'
##' @format An environment.
##' @keywords internal
auth_store <- NULL
makeAuthStore <- function () {
  auth_store <<- new.env(hash = TRUE, parent = emptyenv())
}
makeAuthStore()

##' Kill the active Crunch session
##' @export 
logout <- function () {
    rm(list=ls(envir=auth_store), envir=auth_store)
}

##' Authenticate with the Crunch API
##' @param email the user's email address
##' @param ... additional parameters
##' @export 
login <- function (email, ...) {
    
    out <- crunchAPI("POST", "http://localhost:8080/api/",
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
    auth_store$cookie <- do.call("set_cookies", cookie)
}

getToken <- function () auth_store$cookie