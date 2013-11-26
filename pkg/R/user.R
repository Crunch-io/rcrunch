## Retrieve the URLs to resource collections stored on the user entity
getUserResourceURLs <- function () {
    GET(sessionURL("user_url"))$urls
}
