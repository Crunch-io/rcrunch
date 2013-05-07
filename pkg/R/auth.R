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
##'
##' Note that you can store your Crunch account info in your .Rprofile under
##' "crunch.email" and "crunch.pw" for convenience. If you do so, you can simply
##' \code{login()} to authenticate. For running batch jobs, this could be
##' particularly useful. However, be warned that storing your 
##' password in a plain text file such as .Rprofile is a security risk (though
##' perhaps less so than in every .R script you write), and we
##' cannot officially recommend that you do so.
##'
##' If a password is not supplied (or, if no arguments are supplied and only
##' the \code{crunch.email} is specified in .Rprofile), and you are in an 
##' interactive session, you will be prompted to enter your password. At 
##' present, this is the most secure practice as your password is not stored
##' locally.
##'
##' @param email the email address associated with the user's Crunch account
##' @param password the password associated with the user's Crunch account
##' @param ... additional parameters passed in the authentication. Not 
##' currently supported by the Crunch API.
##' @export 
login <- function (email=getOption("crunch.email"),
                   password=getOption("crunch.pw"), ...) {
    logout()
    out <- crunchAuth(email=email, password=password, ...)
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

##' Validate authentication inputs and then POST to the API
##' @param email character, see \code{\link{login}}
##' @param password character, see \code{\link{login}}
##' @param ... see \code{\link{login}}
crunchAuth <- function (email, password=NULL, ...) {
    if (is.null(email)) {
        stop("Must supply the email address associated with your crunch.io account", call.=FALSE)
    }
    if (is.null(password)) {
        if (interactive()) {
            password <- readline(paste0("Crunch.io password for ", email, ": "))
        } else {
            stop("Must supply a password", call.=FALSE)
        }
    }
    
    POST(getOption("crunch.api.endpoint"), 
        body=basicAuthArgs(email=email, password=password, ...), 
        status.handlers=list(`401`=function (response, user=email) {
            stop(paste("Unable to authenticate", user), call.=FALSE)
        }))
}

##' @importFrom RJSONIO toJSON
basicAuthArgs <- function (...) {
    return(toJSON(list(...)))
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