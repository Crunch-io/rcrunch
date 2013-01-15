getUserURLs <- function () {
    crunchAPI("GET", getAPIroot()$body$user_url)
}