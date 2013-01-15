##' Stay authenticated.
##'
##' The auth store keeps the session token after authentication so that all 
##' API calls can use it.
##'
##' @format An environment.
##' @keywords internal
auth_store <- NULL
logout <- function () {
  auth_store <<- new.env(hash = TRUE, parent = emptyenv())
}
logout()

