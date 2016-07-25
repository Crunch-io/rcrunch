setup.and.teardown <- function (setup, teardown, obj.name=NULL) {
    ContextManager(enter=setup, exit=teardown, as=obj.name,
        error=function (e) expect_error(stop(e$message), "NO ERRORS HERE!"))
}

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
    with(temp.option(crunch.api="/api/root/"), {
        with_mock(
            `httr::GET`=function (url, ...) {
                if (is.null(url)) {
                    stop("No URL found", call.=FALSE)
                }
                url <- unlist(strsplit(url, "?", fixed=TRUE))[1] ## remove query params
                url <- sub("\\/$", ".json", url)
                url <- sub("^\\/", "", url) ## relative to cwd
                return(fakeResponse(url))
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

with_silent_progress <- function (expr) {
    with_mock(
        `utils::txtProgressBar`=function (...) invisible(NULL),
        `utils::setTxtProgressBar`=function (...) invisible(NULL),
        eval.parent(expr)
    )
}

silencer <- temp.option(show.error.messages=FALSE)

assign("seen.things", c(), envir=globalenv())
with_test_authentication <- function (expr) {
    if (run.integration.tests) {
        ## Authenticate.
        suppressMessages(login())
        ## Any time an object is created (201 Location responts), store that URL
        suppressMessages(trace("locationHeader",
            exit=quote({
                if (!is.null(loc)) {
                    seen <- get("seen.things", envir=globalenv())
                    assign("seen.things",
                        c(seen, loc),
                        envir=globalenv())
                }
            }),
            print=FALSE,
            where=crGET))
        # suppressMessages(trace("createDataset",
        #     quote({
        #         # If we care about making unique dataset names, do this:
        #         # body$body$name <- paste(now(), body$body$name)
        #     }),
        #     at=3,
        #     print=FALSE,
        #     where=createSource))
        on.exit({
            suppressMessages(untrace("locationHeader", where=crGET))
            # suppressMessages(untrace("createDataset", where=crGET))
            ## Delete our seen things
            ## We could filter out variables, batches, anything under a dataset
            ## since we're going to delete the datasets
            seen <- get("seen.things", envir=globalenv())
            for (u in seen) {
                try(crDELETE(u), silent=TRUE)
            }
            logout()
        })
        eval.parent(with_silent_progress(expr))
    }
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
    return(setup.and.teardown(
        function () new.dataset.with.setup(df, ...),
        purge.object,
        obj.name
    ))
}

reset.option <- function (opts) {
    ## Don't set any options in the setup, but reset specified options after
    old <- sapply(opts, getOption, simplify=FALSE)
    return(setup.and.teardown(
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
