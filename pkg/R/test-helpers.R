setup.and.teardown <- function (setup, teardown) {
    structure(list(setup=setup, teardown=teardown), class="SUTD")
}

##' @S3method with SUTD
with.SUTD <- function (data, expr, ...) {
    on.exit(data$teardown())
    data$setup()
    eval(substitute(expr), envir=parent.frame())
}

makeFakeHTTPStore <- function () {
    http <<- new.env(hash = TRUE)
    http$GET <- function (url, ...) url
    http$PUT <- function (...) crunchAPI("PUT", ...)
    http$POST <- function (...) crunchAPI("POST", ...)
}

fake.HTTP <- setup.and.teardown(makeFakeHTTPStore, makeHTTPStore)