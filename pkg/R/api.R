##' @importFrom httr GET PUT POST
crunchAPI <- function (http.verb, url, ..., auth.required=TRUE) {
    auth <- getToken()
    if (is.null(auth) && auth.required) {
        stop("Please authenticate first")
    }
    http.verb <- validateVerb(http.verb)
    http.verb <- match.fun(http.verb)
    
    http.verb(url, ..., config=list(auth))
}

validateVerb <- function (x) {
    if (!is.character(x) || length(x) != 1) {
        stop("Must supply exactly one HTTP verb (character)")
    }
    supported.verbs <- c("GET", "PUT", "POST")
    x <- intersect(x, supported.verbs)
    if (length(x)==0) {
        stop("Unsupported verb")
    }
    return(x)
}