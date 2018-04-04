library(httptest)

run.integration.tests <- Sys.getenv("INTEGRATION") == "TRUE"

skip_locally <- function (...) {
    if (grepl("^https?://local\\.", getOption("crunch.api"))) {
        skip(...)
    }
}

loadCube <- function (filename) {
    crunch:::CrunchCube(jsonlite::fromJSON(filename, simplifyVector=FALSE)$value)
}

cubify <- function (..., dims) {
    ## Make readable test expectations for comparing cube output
    ## Borrowed from Cube arrays, fixtures and cubes come in row-col-etc. order,
    ## not column-major. Make array, then aperm the array back to order
    data <- c(...)
    d <- rev(vapply(dims, length, integer(1), USE.NAMES=FALSE))

    out <- array(data, dim=d)
    if (length(dims) > 1) {
        ap <- seq_len(length(dims))
        ap <- rev(ap)
        out <- aperm(out, ap)
    }
    dimnames(out) <- dims
    return(out)
}

## Contexts
with_mock_crunch <- function (expr) {
    env <- parent.frame()
    with(temp.options(crunch.api="https://app.crunch.io/api/",
                      httptest.mock.paths=c(".", "../inst/", system.file(package="crunch"))), {
        with_mock_api({
            try(crunch:::warmSessionCache())
            eval(expr, envir=env)
        })
    })
}

with_POST <- function (resp, expr) {
    ## Mock a POST that returns something, like a Location header pulled from 201
    force(resp)
    with_mock(`crunch::crPOST`=function (...) resp, eval.parent(expr))
}

with_PATCH <- function (resp, expr) {
    ## Mock a PATCH that returns something, or nothing
    force(resp)
    with_mock(`crunch::crPATCH`=function (...) resp, eval.parent(expr))
}

with_DELETE <- function (resp, expr) {
    ## Mock a DELETE that returns something, or nothing
    force(resp)
    with_mock(`crunch::crDELETE`=function (...) resp, eval.parent(expr))
}

assign("entities.created", c(), envir=globalenv())

test_options <- temp.options(
    # grab env or options
    # use test.api or R_TEST_API if it's available, if not use local
    crunch.api=crunch::envOrOption("test.api",
                                   "http://local.crunch.io:8080/api/"),

    crunch.email=crunch::envOrOption("test.user"),
    crunch.pw=crunch::envOrOption("test.pw"),
    crunch.show.progress=FALSE,
    folders.enabled = TRUE
)

with_test_authentication <- function (expr) {
    if (run.integration.tests) {
        env <- parent.frame()

        with(test_options, {
            ## Authenticate.
            try(suppressMessages(login()))
            on.exit({
                ## Delete our seen things
                purgeEntitiesCreated()
                logout()
            })
            ## Any time an object is created (201 Location responts), store
            ## that URL
            tracer <- quote({
                if (!is.null(loc)) {
                    seen <- get("entities.created", envir=globalenv())
                    assign("entities.created",
                           c(seen, loc),
                           envir=globalenv())
                }
            })
            with_trace("locationHeader", exit=tracer, where=crGET, expr={
                ## Wrap this so that we can generate a test failure if
                ## there's an error rather than just halt the process
                tryCatch(eval(expr, envir=env),
                         error=function (e) {
                             test_that("There are no test code errors", {
                                 expect_error(stop(e$message), NA)
                             })
                         })
            })
        })
    }
}

purgeEntitiesCreated <- function () {
    seen <- get("entities.created", envir=globalenv())
    ds.urls <- grep("/datasets/(.*?)/$", seen, value=TRUE)
    if (length(ds.urls)) {
        ignore <- Reduce("|", lapply(ds.urls, function (x) {
            substr(seen, 1, nchar(x)) == x & seen != x
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


## Global teardown code
with_test_authentication({
    datasets.start <- urls(datasets())
    users.start <- urls(crunch:::getUserCatalog())
    projects.start <- urls(projects())
})

crunch_test_teardown_check <- function () {
    with_test_authentication({
        datasets.end <- urls(datasets())
        leftovers <- setdiff(datasets.end, datasets.start)
        if (length(leftovers)) {
            message(length(leftovers),
                 " dataset(s) created and not destroyed: ",
                 crunch:::serialPaste(dQuote(names(datasets()[leftovers]))))
        }
        users.end <- urls(crunch:::getUserCatalog())
        leftovers <- setdiff(users.end, users.start)
        if (length(leftovers)) {
            message(length(leftovers),
                 " users(s) created and not destroyed: ",
                 crunch:::serialPaste(dQuote(names(crunch:::getUserCatalog()[leftovers]))))
        }
        projects.end <- urls(projects())
        leftovers <- setdiff(projects.end, projects.start)
        if (length(leftovers)) {
            message(length(leftovers),
                 " projects(s) created and not destroyed: ",
                 crunch:::serialPaste(dQuote(names(projects()[leftovers]))))
        }
    })
}
