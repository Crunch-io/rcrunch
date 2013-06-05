setup.and.teardown <- function (setup, teardown) {
    structure(list(setup=setup, teardown=teardown), class="SUTD")
}

##' @S3method with SUTD
with.SUTD <- function (data, expr, ...) {
    env <- parent.frame()
    on.exit(data$teardown())
    data$setup()
    eval(substitute(expr), envir=parent.frame())
}

addFakeHTTPVerbs <- function () {
    # http_verbs <<- new.env(hash = TRUE, parent = emptyenv())
    http_verbs$GET <- function (url, ...) url
    http_verbs$PUT <- function (...) crunchAPI("PUT", ...)
    http_verbs$POST <- function (...) crunchAPI("POST", ...)
}

fake.HTTP <- setup.and.teardown(addFakeHTTPVerbs, addRealHTTPVerbs)

## Setup-teardown
test.authentication <- setup.and.teardown(
    function () suppressMessages(login()), 
    logout)