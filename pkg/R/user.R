getUserURLs <- function () {
    crunchAPI("GET", sessionURL("user_url"))$urls
}
