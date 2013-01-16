getUserURLs <- function () {
    crunchAPI("GET", getAPIroot()$urls$user_url)
}