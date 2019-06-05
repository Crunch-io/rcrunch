# nolint start
# we don't lint this file because we commit a few violations so that test envs
# can be set up and torn down easily.
library(httptest)

run.integration.tests <- Sys.getenv("INTEGRATION") == "TRUE"


skip_on_local_backend <- function(message) {
    # if we are trying to skip when the backend is local
    if (grepl("^https?://local\\.", getOption("crunch.api"))) {
        return(skip(paste("Skipping with local backend:", message)))
    }
}

skip_on_local_env <- function(message) {
    jenkins <- identical(Sys.getenv("JENKINS_HOME"), "true")
    travis <- identical(Sys.getenv("TRAVIS"), "true")
    cran <- !identical(Sys.getenv("NOT_CRAN"), "true")
    appveyor <- identical(Sys.getenv("APPVEYOR"), "True")

    # if we are trying to skip when the tests are being run locally
    if (!any(jenkins, travis, cran, appveyor)) {
        return(skip(paste("Skipping locally:", message)))
    }
}

decompress_fixtures <- function(dest = tempdir()) {
    untar(find_file("cubes.tgz"), compressed = TRUE, exdir = tempdir())
}

cubePath <- function(filename) {
    # check the temp place
    file <- file.path(tempdir(), filename)

    # if it's not there, see if it's in the package this should only be needed
    # for backwards compatibility wit hchild packages
    if (!file.exists(file)) {
        file <- system.file(filename, package = "crunch")
    }

    if (nchar(file) > 0) {
        filename <- file
    }

    return(filename)
}

loadCube <- function(filename) {
    # check the temp place
    filename <- cubePath(filename)


    # if the cube json has a value name, it has full metadata and we need to
    # extract only the value
    cube_json <- jsonlite::fromJSON(filename, simplifyVector = FALSE)
    if ("value" %in% names(cube_json)) {
        cube_json <- cube_json$value
    }

    return(crunch:::CrunchCube(cube_json))
}

cubify <- function(..., dims) {
    ## Make readable test expectations for comparing cube output
    ## Borrowed from Cube arrays, fixtures and cubes come in row-col-etc. order,
    ## not column-major. Make array, then aperm the array back to order
    data <- c(...)
    d <- rev(vapply(dims, length, integer(1), USE.NAMES = FALSE))

    out <- array(data, dim = d)
    if (length(dims) > 1) {
        ap <- seq_len(length(dims))
        ap <- rev(ap)
        out <- aperm(out, ap)
    }
    dimnames(out) <- dims
    return(out)
}

## Contexts
with_mock_crunch <- function(expr) {
    opts <- temp.options(
        crunch.api = "https://app.crunch.io/api/",
        httptest.mock.paths = c(".", "../inst/", system.file(package = "crunch"))
    )
    with(
        opts,
        with_mock_API(expr)
    )
}

with_POST <- function(resp, expr) {
    ## Mock a POST that returns something, like a Location header pulled from 201
    force(resp)
    with_mock(`crunch::crPOST` = function(...) resp, eval.parent(expr))
}

with_PATCH <- function(resp, expr) {
    ## Mock a PATCH that returns something, or nothing
    force(resp)
    with_mock(`crunch::crPATCH` = function(...) resp, eval.parent(expr))
}

with_DELETE <- function(resp, expr) {
    ## Mock a DELETE that returns something, or nothing
    force(resp)
    with_mock(`crunch::crDELETE` = function(...) resp, eval.parent(expr))
}

assign("entities.created", c(), envir = globalenv())

test_options <- temp.options(
    # grab env or options
    # use test.api or R_TEST_API if it's available, if not use local
    crunch.api = crunch::envOrOption(
        "test.api",
        "http://local.crunch.io:8080/api/"
    ),

    crunch.email = crunch::envOrOption("test.user"),
    crunch.pw = crunch::envOrOption("test.pw"),
    crunch.show.progress = FALSE
)

with_test_authentication <- function(expr) {
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
                    seen <- get("entities.created", envir = globalenv())
                    assign("entities.created",
                        c(seen, loc),
                        envir = globalenv()
                    )
                }
            })
            with_trace("locationHeader", exit = tracer, where = crGET, expr = {
                ## Wrap this so that we can generate a test failure if
                ## there's an error rather than just halt the process
                tryCatch(eval(expr, envir = env),
                    error = function(e) {
                        test_that("There are no test code errors", {
                            expect_error(stop(e$message), NA)
                        })
                    }
                )
            })
        })
    }
}

with_timing <- function (global.varname, expr) {
    rightnow <- Sys.time()
    totaltime <- get0(global.varname, envir = globalenv(), ifnotfound=0)
    on.exit(assign(
        global.varname,
        totaltime + difftime(Sys.time(), rightnow, "seconds"),
        envir = globalenv()
    ))
    eval.parent(expr)
}

purgeEntitiesCreated <- function() {
    with_timing("cleanup.runtime", {
        seen <- get("entities.created", envir = globalenv())
        ds.urls <- grep("/datasets/(.*?)/$", seen, value = TRUE)
        if (length(ds.urls)) {
            ## Filter out variables, batches, anything under a dataset
            ## since we're going to delete the datasets
            ignore <- Reduce("|", lapply(ds.urls, function(x) {
                substr(seen, 1, nchar(x)) == x & seen != x
            }))
            seen <- seen[!ignore]
        }
        for (u in seen) {
            ## TODO: use curl::curl_fetch_multi to background/parallelize these?
            ## If so, might need to wait for them to complete (just in parallel)
            ## rather than fire-and-forget so that other synchronous code doesn't fail
            try(crDELETE(u), silent = TRUE)
        }
        assign("entities.created", c(), envir = globalenv())
        invisible()
    })
}

## Substitute for testthat::describe or similar, just a wrapper around a context
## to force deleting stuff it creates sooner
whereas <- function(...) {
    on.exit(purgeEntitiesCreated())
    eval.parent(...)
}


## Global teardown code
with_test_authentication({
    datasets.start <- urls(datasets())
    users.start <- urls(crunch:::getUserCatalog())
    projects.start <- urls(projects())
})

crunch_test_teardown_check <- function() {
    with_timing("cleanup.runtime", {
        with_test_authentication({
            datasets.end <- urls(datasets())
            leftovers <- setdiff(datasets.end, datasets.start)
            if (length(leftovers)) {
                message(
                    length(leftovers),
                    " dataset(s) created and not destroyed: ",
                    crunch:::serialPaste(dQuote(names(datasets()[leftovers])))
                )
            }
            users.end <- urls(crunch:::getUserCatalog())
            leftovers <- setdiff(users.end, users.start)
            if (length(leftovers)) {
                message(
                    length(leftovers),
                    " users(s) created and not destroyed: ",
                    crunch:::serialPaste(dQuote(names(crunch:::getUserCatalog()[leftovers])))
                )
            }
            projects.end <- urls(projects())
            leftovers <- setdiff(projects.end, projects.start)
            if (length(leftovers)) {
                message(
                    length(leftovers),
                    " projects(s) created and not destroyed: ",
                    crunch:::serialPaste(dQuote(names(projects()[leftovers])))
                )
            }
        })
    })
    cat("Total teardown: ")
    print(get("cleanup.runtime", envir = globalenv()))
}
# nolint end
