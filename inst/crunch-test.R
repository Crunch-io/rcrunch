library(httptest)

run.integration.tests <- Sys.getenv("INTEGRATION") == "TRUE"

if (crunch::envOrOption("test.verify.ssl", TRUE, expect_lgl = TRUE) == FALSE) {
    crunch::set_crunch_config(httr::config(ssl_verifyhost = FALSE, ssl_verifypeer = FALSE), update = TRUE)
}

skip_on_local_backend <- function(message) {
    # if we are trying to skip when the backend is local
    if (grepl("^https?://local\\.", getOption("crunch.api"))) {
        return(skip(paste("Skipping with local backend:", message)))
    }
}

skip_on_local_env <- function(message) {
    jenkins <- identical(Sys.getenv("JENKINS_HOME"), "true")
    cran <- !identical(Sys.getenv("NOT_CRAN"), "true")
    travis <- identical(Sys.getenv("TRAVIS"), "true")
    appveyor <- identical(Sys.getenv("APPVEYOR"), "True")
    github <- identical(Sys.getenv("GITHUB_ACTION"), "true")

    # if we are trying to skip when the tests are being run locally
    if (!any(jenkins, cran, travis, appveyor, github)) {
        return(skip(paste("Skipping locally:", message)))
    }
}

decompress_fixtures <- function(dest = tempdir()) {
    untar(system.file("mocks.tgz", package = "crunch"), exdir = tempdir())

    if (!identical(Sys.getenv("NOT_CRAN"), "true"))  return()
    # --- If not on CRAN, try checking if the mocks are totally up-to-date
    potential_path <- file.path(testthat::test_path("..", "..", "mocks"))
    if (dir.exists(potential_path)) {
        current_files <- list.files(potential_path, recursive = TRUE)
        current_file_info <- data.frame(
            name = current_files,
            md5 = tools::md5sum(file.path(potential_path, current_files)),
            stringsAsFactors = FALSE,
            row.names = NULL
        )
        current_file_info <- current_file_info[order(current_file_info$name), ]

        temp_files <- list.files(file.path(tempdir(), "mocks"), recursive = TRUE)
        temp_file_info <- data.frame(
            name = temp_files,
            md5 = tools::md5sum(file.path(tempdir(), "mocks", temp_files)),
            stringsAsFactors = FALSE,
            row.names = NULL
        )
        temp_file_info <- temp_file_info[order(temp_file_info$name), ]

        if (!isTRUE(all.equal(current_file_info, temp_file_info, check.attributes = FALSE))) {
            warning(
                "mocks tarball looks out of date. Run command `make compress-fixtures` to update"
            )
        }
    } else {
        warning(paste0(
            "Could not find cubes directory so could not check if mock fixures are up to date. ",
            "You can make sure they're updated by running `make compress-fixtures`."
        ))
    }
}

cubePath <- function(filename) {
    # check the temp place
    file <- file.path(tempdir(), "mocks", filename)

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
        httptest.mock.paths = c(".", "../mocks/", file.path(tempdir(), "mocks"))
    )
    with(
        opts,
        with_mock_api(expr)
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
assign("all.dataset.entities.created", c(), envir = globalenv())

test_options <- temp.options(
    # grab env or options
    # use test.api or R_TEST_API if it's available, if not use local
    crunch = list(
        crunch.api = crunch::envOrOption(
            "test.api",
            "http://local.crunch.io:8080/api/"
        ),
        crunch.api.key = Sys.getenv("CRUNCH_TEST_API_KEY"),
        crunch.show.progress = FALSE,
        crunch.verify_ssl = crunch::envOrOption("test.verify_ssl", TRUE, expect_lgl = TRUE),
        message.auth.info = TRUE
    )
)

with_test_authentication <- function(expr) {
    if (run.integration.tests) {
        env <- parent.frame()

        with(test_options, {
            ## Authenticate.
            on.exit({
                httpcache::clearCache()
                ## Delete our seen things
                purgeEntitiesCreated()
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
                ## (eg make sure we run the test teardown)
                eval(expr, envir = env)
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
        seen.ds <- get0("all.dataset.entities.created", envir = globalenv())
        ds.urls <- grep("/datasets/(.*?)/$", seen, value = TRUE)
        assign("all.dataset.entities.created", c(seen.ds, ds.urls), envir = globalenv())

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
            message(
                length(unique(get0("all.dataset.entities.created", envir = globalenv()))),
                " datasets created during testing"
            )

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

# Another level of caching for full datasets because even with httpcache
# storing JSON in memory, loading the two most frequent datasets was still
# taking ~10% of test time because we do it so often during testing.
ds_cache_env <- new.env(parent = emptyenv())

cachedLoadDataset <- function(dataset, ...) {
    if (length(list(...)) != 0) {
        stop("non-dataset arguments ignored in cached datasets")
    }
    if (!dataset %in% names(ds_cache_env)) {
        # Don't need `with_mock_crunch()` because caller is already inside it
        ds_cache_env[[dataset]] <- loadDataset(dataset)
    }
    ds_cache_env[[dataset]]
}


# Make compressed fixtures available to downstream packages
decompress_fixtures()
