##' Stay authenticated.
##'
##' The auth store keeps the session token after authentication so that all 
##' API calls can use it.
##'
##' @format An environment.
##' @keywords internal
auth_store <- NULL
.log_out <- function () {
  auth_store <<- new.env(hash = TRUE, parent = emptyenv())
}
.log_out()

##' Kill the active Crunch session
##' @export 
logout <- function () {
    rm(list=ls(envir=auth_store), envir=auth_store)
}

login <- function (email) {
    auth_store$cookie <- list(email)
    invisible()
}