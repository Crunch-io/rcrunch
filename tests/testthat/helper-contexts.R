

setup.and.teardown <- function (setup, teardown, obj.name=NULL) {
    ContextManager(enter=setup, exit=teardown, as=obj.name,
        error=function (e) expect_error(stop(e$message), "NO ERRORS HERE!"))
}

with_mock_HTTP <- function (expr) {
    with(temp.option(crunch.api="/api/root.json"), {
        with_mock(
            `httr::GET`=function (url, ...) {
                if (is.null(url)) {
                    stop("No URL found", call.=FALSE)
                }
                url <- unlist(strsplit(url, "?", fixed=TRUE))[1] ## remove query params
                url <- sub("\\/$", ".json", url)
                url <- sub("^\\/", "", url) ## relative to cwd
                out <- handleShoji(fromJSON(url, simplifyVector=FALSE))
                return(list(
                    status_code=200,
                    times=structure(nchar(url), .Names="total"),
                    request=list(method="GET", url=url),
                    response=out
                ))
            },
            `crunch::handleAPIresponse`=function (response, special.statuses=list()) {
                response <- handleAPIerror(response)
                return(response$response)
            },
            `httr::PUT`=function (url, body, ...) halt("PUT ", url, " ", body),
            `httr::PATCH`=function (url, body, ...) halt("PATCH ", url, " ", body),
            `httr::POST`=function (url, body, ...) halt("POST ", url, " ", body),
            `httr::DELETE`=function (url, ...) halt("DELETE ", url),
            eval.parent(try(warmSessionCache())),
            eval.parent(expr)
        )
    })
}

## Mock backend for no connectivity
without_internet <- function (expr) {
    with_mock(
        `httr::GET`=function (url, ...) halt("GET ", url),
        `httr::PUT`=function (url, body, ...) halt("PUT ", url, " ", body),
        `httr::PATCH`=function (url, body, ...) halt("PATCH ", url, " ", body),
        `httr::POST`=function (url, body, ...) halt("POST ", url, " ", body),
        `httr::DELETE`=function (url, ...) halt("DELETE ", url),
        eval.parent(expr)
    )
}

silencer <- temp.option(show.error.messages=FALSE)

## Auth setup-teardown
test.authentication <- setup.and.teardown(
    function () suppressMessages(login()),
    logout)

uniqueDatasetName <- now

## Create a test dataset and then destroy it after tests
datasets_to_purge <- c()
new.dataset.with.setup <- function (df=NULL, ...) {
    unique.name <- uniqueDatasetName()
    if (is.dataset(df)) {
        ## Passing a dataset already made in, just to ensure its cleanup
        ## Just return it
        out <- df
    } else if (is.null(df)) {
        out <- createDataset(name=unique.name, ...)
    } else {
        out <- suppressMessages(newDataset(df, name=unique.name, ...))
    }
    datasets_to_purge <<- c(datasets_to_purge, self(out))
    return(out)
}

purge.dataset <- function () {
    len <- length(datasets_to_purge)
    if (len) {
        try(crDELETE(datasets_to_purge[len]), silent=TRUE)
        datasets_to_purge <<- datasets_to_purge[-len]
    }
}

test.dataset <- function (df=NULL, obj.name="ds", ...) {
    return(setup.and.teardown(
        function () new.dataset.with.setup(df, ...),
        purge.dataset,
        obj.name
    ))
}

reset.option <- function (opts) {
    ## Don't set any options in the setup, but reset specified options after
    old <- sapply(opts, getOption, simplify=FALSE)
    return(setup.and.teardown(
        function () NULL,
        function () do.call(options, old)
    ))
}

uniqueEmail <- function () paste0("test+", as.numeric(Sys.time()), "@crunch.io")
users_to_purge <- c()
new.user.with.setup <- function (email=uniqueEmail(), name=email, ...) {
    u.url <- invite(email, name=name, notify=FALSE, ...)
    users_to_purge <<- c(users_to_purge, u.url)
    return(u.url)
}

purge.user <- function () {
    len <- length(users_to_purge)
    if (len) {
        u.url <- users_to_purge[len]
        try(crDELETE(u.url))
        users_to_purge <<- users_to_purge[-len]
    }
}

test.user <- function (email=uniqueEmail(), name=email, obj.name="u", ...) {
    return(setup.and.teardown(
        function () new.user.with.setup(email, name, ...),
        purge.user,
        obj.name
    ))
}
