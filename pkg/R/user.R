getUserURLs <- function () {
    GET(sessionURL("user_url"))$urls
}
