getSummary <- function (x) {
    url <- x@urls$summary_url
    if (is.null(url)) stop("No summary available", call.=FALSE)
    return(GET(url))
}

