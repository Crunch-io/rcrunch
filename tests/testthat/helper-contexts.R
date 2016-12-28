fakeResponse <- function (url="", status_code=200, headers=list(), json=NULL) {
    ## Return something that looks enough like an httr 'response'
    if (!is.null(json)) {
        cont <- charToRaw(toJSON(json))
    } else {
        cont <- readBin(url, "raw", 4096)
    }
    structure(list(
        url=url,
        status_code=status_code,
        times=structure(nchar(url), .Names="total"),
        request=list(method="GET", url=url),
        headers=modifyList(list(`Content-Type`="application/json"), headers),
        content=cont
    ), class="response")
}

with_mock_HTTP <- function (expr) {
    with(temp.option(crunch.api="api/root/"), {
        without_internet({
            with_mock(
                `httr::GET`=function (url, ...) {
                    if (is.null(url)) {
                        stop("No URL found", call.=FALSE)
                    }
                    url <- unlist(strsplit(url, "?", fixed=TRUE))[1] ## remove query params in the URL
                    q <- list(...)$query
                    ext <- ".json"
                    if (!is.null(q)) {
                        ## There's a query.
                        ## Hash it, take the first 6 chars, and add to the filename
                        ext <- paste0("-", substr(digest::digest(q), 1, 6), ext)
                    }
                    url <- sub("\\/$", ext, url)
                    return(fakeResponse(url))
                },
                `crunch:::absoluteURL`=function (urls, base) {
                    ## Absolute URLs with fake backend start with "api/..."
                    if (length(urls) && !any(startsWith(urls, "api"))) {
                        urls <- .abs.urls(urls, base)
                    }
                    return(urls)
                },
                eval.parent(try(warmSessionCache())),
                eval.parent(expr)
            )
        })
    })
}

with_silent_progress <- function (expr) {
    with_mock(
        `utils::txtProgressBar`=function (...) invisible(NULL),
        `utils::setTxtProgressBar`=function (...) invisible(NULL),
        eval.parent(expr)
    )
}

silencer <- temp.option(show.error.messages=FALSE)

assign("entities.created", c(), envir=globalenv())
with_test_authentication <- function (expr) {
    if (run.integration.tests) {
        ## Authenticate.
        suppressMessages(login())
        ## Any time an object is created (201 Location responts), store that URL
        suppressMessages(trace("locationHeader",
            exit=quote({
                if (!is.null(loc)) {
                    seen <- get("entities.created", envir=globalenv())
                    assign("entities.created",
                        c(seen, loc),
                        envir=globalenv())
                }
            }),
            print=FALSE,
            where=crGET))
        on.exit({
            suppressMessages(untrace("locationHeader", where=crGET))
            # suppressMessages(untrace("createDataset", where=crGET))
            ## Delete our seen things
            purgeEntitiesCreated()
            logout()
        })
        ## Wrap this so that we can generate a test failure if there's an error
        ## rather than just halt the process
        tryCatch(eval.parent(with_silent_progress(expr)),
            error=function (e) {
                test_that("There are no test code errors", {
                    expect_error(stop(e$message), NA)
                })
            })
    }
}

purgeEntitiesCreated <- function () {
    seen <- get("entities.created", envir=globalenv())
    ds.urls <- grep("/datasets/(.*?)/$", seen, value=TRUE)
    if (length(ds.urls)) {
        ignore <- Reduce("|", lapply(ds.urls, function (x) {
            startsWith(seen, x) & seen != x
        }))
        seen <- seen[!ignore]
    }
    for (u in seen) {
        ## We could filter out variables, batches, anything under a dataset
        ## since we're going to delete the datasets
        try(crDELETE(u), silent=TRUE)
    }
    assign("entities.created", c(), envir=globalenv())
    invisible()
}

## Substitute for testthat::describe or similar, just a wrapper around a context
## to force deleting stuff it creates sooner
whereas <- function (...) {
    on.exit(purgeEntitiesCreated())
    eval.parent(...)
}

uniqueDatasetName <- now

## Create a test dataset and then destroy it after tests
objects_to_purge <- c()
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
    objects_to_purge <<- c(objects_to_purge, self(out))
    return(out)
}

purge.object <- function () {
    len <- length(objects_to_purge)
    if (len) {
        try(crDELETE(objects_to_purge[len]), silent=TRUE)
        objects_to_purge <<- objects_to_purge[-len]
    }
}

test.dataset <- function (df=NULL, obj.name="ds", ...) {
    return(ContextManager(
        function () new.dataset.with.setup(df, ...),
        purge.object,
        as=obj.name
    ))
}

reset.option <- function (opts) {
    ## Don't set any options in the setup, but reset specified options after
    old <- sapply(opts, getOption, simplify=FALSE)
    return(ContextManager(
        null,
        function () do.call(options, old)
    ))
}

uniqueEmail <- function () paste0("test+", as.numeric(Sys.time()), "@crunch.io")
testUser <- function (email=uniqueEmail(), name=paste("Ms.", email, "User"), ...) {
    u.url <- invite(email, name=name, notify=FALSE, ...)
    return(UserEntity(crGET(u.url)))
}

testProject <- function (name="", ...) {
    name <- paste0(name, as.numeric(Sys.time()))
    p <- session()$projects
    p[[name]] <- list(...)
    return(refresh(p)[[name]])
}
