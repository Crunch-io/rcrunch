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
    with_mocked_bindings(crPOST = function(...) resp, eval.parent(expr), .package = "crunch")
}

with_PATCH <- function(resp, expr) {
    ## Mock a PATCH that returns something, or nothing
    force(resp)
    with_mocked_bindings(crPATCH = function(...) resp, eval.parent(expr), .package = "crunch")
}

with_DELETE <- function(resp, expr) {
    ## Mock a DELETE that returns something, or nothing
    force(resp)
    with_mocked_bindings(crDELETE = function(...) resp, eval.parent(expr), .package = "crunch")
}

assign("entities.created", c(), envir = globalenv())

test_options <- temp.options(
    # grab env or options
    # use test.api or R_TEST_API if it's available, if not use local
    crunch = list(
        crunch.api = crunch::envOrOption(
            "test.api",
            "http://local.crunch.io:8080/api/"
        ),
        crunch.api.key = envOrOption(
            "test.api.key",
            Sys.getenv("CRUNCH_TEST_API_KEY") # Weird default for historic reasons
        ),
        crunch.show.progress = FALSE,
        crunch.verify_ssl = crunch::envOrOption("test.verify_ssl", TRUE, expect_lgl = TRUE),
        message.auth.info = TRUE,
        crunch.default.project = "RCRUNCH_TESTS"
    )
)

with_test_authentication <- function(expr) {
    if (run.integration.tests) {
        env <- parent.frame()

        with(test_options, {
            ## Authenticate.
            newProject(envOrOption("crunch.default.project"))
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

# Another level of caching for full datasets because even with httpcache
# storing JSON in memory, loading the two most frequent datasets was still
# taking ~10% of test time because we do it so often during testing.
ds_cache_env <- new.env(parent = emptyenv())

cachedLoadDataset <- function(dataset, ...) {
    if (length(list(...)) != 0) {
        stop("non-dataset arguments ignored in cached datasets")
    }
    if (!dataset %in% names(ds_cache_env)) {
        # replicate old dataset by name behavior
        ds_id <- switch(
            dataset,
            "test ds" = "1",
            "ECON.sav" = "3",
            "test ds deck" = "4",
            "Vegetables example" = "veg",
            stop("Update cachedLoadDataset name->id crosswalk")
        )

        # Don't need `with_mock_crunch()` because caller is already inside it
        ds_cache_env[[dataset]] <- loadDataset(ds_id, project = NULL)
    }
    ds_cache_env[[dataset]]
}

fixture_ds_cw <- c(
    active_filter = "0c6ceeefe14e462b8cfae8ab0cebf813",
    add_subvariable = "2c41f6d5f1af4274830aaf5d4a03a83a",
    add_variable = "6d64a875792045ab92261ff1b6275ee1",
    add_variable_definition = "e638816c26ce40c8b12abf8904f5f86b",
    append_conflicts_array_numeric_part1 = "67e02b2cae1743c9a4d217ca212826bd",
    append_conflicts_array_numeric_part2 = "844d6b50b0f745da917ec1592e35ad75",
    append_conflicts_text_numeric_part1 = "8dfb2e17c9ee47548f317858fba3ea33",
    append_conflicts_text_numeric_part2 = "aa02e38ce1fc492c9988cc87e506b40a",
    append_debug_arrays_diff_part1 = "c8d5ec77ad204d1688d2773c3c5ea76b",
    append_debug_arrays_diff_part2 = "d75ba31860314e1fbcbf8e6815add488",
    append_debug_arrays_same_part1 = "47d8ab5b69574360912ef503bd50f989",
    append_debug_arrays_same_part2 = "752ce0b17f8f4ca2a9a5d00490d3ef05",
    append_debug_derived_part1 = "2f561a7f043244dcbf833c7306a3ba93",
    append_debug_derived_part2 = "2275a826510a4a71bc1b56b9d34c8d7a",
    append_debug_exclusion_part1 = "15a3a5355adf4f1ea20545d7c2dcaa6e",
    append_debug_exclusion_part2 = "69c5d06ecf074d89b57eb249ea85f1ef",
    append_sparse_part1 = "e7dd02fd6de3425a8db40fa0c0d37a01",
    append_sparse_part2 = "d6148e7fad324f5ab5b9c2e1454dbec5",
    append_subset_part1 = "635a4702312246bda874fec1b96b9946",
    append_subset_part2 = "ace8960d3d6f4ad6982002834a12c226",
    append_subvariables_diff_part1 = "f84c055c1682406b84d385b02046b517",
    append_subvariables_diff_part2 = "820717c205c1474686837075b25d9e73",
    appending_arrays_mismatch_p1 = "3d687047a8ca43258343306e704a0685",
    appending_arrays_mismatch_p2 = "e47ec44ae32f4e32ba77bec8b1bd4114",
    appending_arrays_revert_p1 = "e43c3343c96b4f529ba6e60a6d3cc0d1",
    appending_arrays_revert_p2 = "c662d041ba8b4b8c93f32cf87c9e3e9d",
    appending_part1 = "6fbf82172a5645a09b74856778166ac1",
    appending_part2 = "eb12789e959347759aaf35e28f0329f1",
    as_data_frame_mr = "27298e2c0608444a9196c71845a023e4",
    `case-variables` = "ad425652b67c4286987f0e6aa5002e03",
    `case-when` = "bcbe81f6ceb7460da664a251698a13c4",
    `change-categories` = "45d82eb49f164518b85a07176924e43f",
    `change-categories-array` = "a4140edb537d4ff7aa4c06a12fb9fea1",
    `combine-categories` = "e3bb703642ba435494bd9be77fb7fe6c",
    `combine-responses` = "fa27301bb75d487bbc0cdc14e18f3052",
    `copy-variable` = "e3e9297c85424a618070ebac66c3db80",
    `cube-collapse-dimensions` = "910355e0b17e419f910571d01bc3798d",
    `cube-dims` = "a08152c1cea342328f92b78b8a70cdb8",
    `cube-errors` = "b8b648ec355647c0a4515c6bc32255a3",
    `cube-index-table` = "f8e14972d9854353a03cf9ca67e15e42",
    `cube-mr` = "adfbe6ced6c64f31923ca3499c09a61e",
    `cube-nway` = "8baec2dfea9440378de082bd66f6f664",
    `cube-transforms` = "274916c38c6d48bda9c14d2a30113946",
    `cube-with-array` = "0d4b074ddfe5496eae8a86d0f4eda81a",
    `cube-with-expression` = "0c197fcfaf284dcb919a73f2ce08d609",
    `cubes-main` = "6451e5e2de0b4755bd221212a678b32c",
    `cubes-scorecard` = "2fb5096f26e646a2abf5665129be5ed0",
    cut = "d4e3ad00ef8041aaab7aef71131d407e",
    `deep-copy1` = "1f392426935a4908a49f851ffb7c6cf8",
    `deep-copy2` = "f0bd7149b3324c358aedc2bd3495a903",
    derive = "55e44d35d15f417b88ff3d363f6032ca",
    `derive-array1` = "8ae84d78c4ce4e639887da94403a1a58",
    `derive-array2` = "4e56490e273547aa9e81e878c7aef44d",
    `derive-array3` = "11d93672e5b0458891c9a751df786807",
    `derive-debug` = "a1b7394669104ea495373acee90735d8",
    `drop-rows1` = "e311753d8b804821a9de627d17260767",
    `drop-rows2` = "1e02d0b56b084b96a0c4b8255579b6c8",
    `exclusion-filter` = "630f76efd3d14bd29fb832dd6288732d",
    `export-dataset` = "f67116a3fc884abe8d299295a07aad1c",
    expressions = "3be0918ed1d249609f4c2ea0b23c6e32",
    filters = "211c312b67484a6882c05b2806db1efd",
    `flip-array` = "f483b30a2014453883fc2e346d743c03",
    folders = "89cc4c26a8f44d8ca35e770aeb41b0d5",
    fork1 = "910aba1f40ef4db49f916c62700fdd05",
    fork2 = "732ba01fd0a04581bd7efed946673eb9",
    `hide-variables` = "bb0a3a8600744869a1370968671eeff7",
    `hide-variables-array` = "0ed58b95bb254fea84c9d1909fc7f759",
    `hide-variables-single` = "7dafc7a9cf514108bd2e67b44ceea70a",
    interact = "4725c795663f4801811caddcd6d4d198",
    lock = "2ba6e4cea6fe46a8afbf5e265b362b72",
    `merge-datasets1` = "3961b0503f5a4e9b9ab9e3274b45da47",
    `merge-datasets3` = "2a96aeb4241a4653a5790e449ba370d9",
    `merge-datasets4` = "7695209b58744388bdf082a444287a90",
    `merge-datasets5` = "fd9cf13b32424982a81bc89d9ea2af55",
    `merge-datasets6` = "713d5d0d854a4cd2bab63a528a90e605",
    shoji = "f11df12f4d274fb38721e8db30d433f2",
    subvariables = "27032f3255a34051be167bbeaa8c6c8d",
    update = "40edd784a5fa4fa9b4cd92971fd02da4",
    `update-array1` = "9e4c52a880ea454da4df562a702a5959",
    `update-array2` = "7117bad9a0f34a5cb027a7b9c88c6eca",
    `update-errors` = "bb4b9b47f89e427cb973fd1deba7acac",
    utftesting = "d054c7c649734110b035d2cc2291a97d",
    `variable-as-methods` = "2b1b5b86475a49d0a74973bcd0a69762",
    `variable-catalog` = "93423b64c3bc42aab14e1adfb35ff769",
    `variable-delete` = "588e00ecef9e4dae9243adc6e2b5cde9",
    `variable-delete-array` = "3e050eaba6204ba88d533ecd8281e07e",
    `variable-folders` = "c8e68f0b745a461d8a947974b88ecf80",
    `variable-metadata` = "8e7d7982d577423bbfad84d3e57253a3",
    `variable-order` = "263bff4c952f4ca5bd3f66985ea2a72c",
    `variable-transforms` = "aa7770878f794788a996cab37a4a5ddc",
    `variable-type` = "3e30e58b6ac7464d99fe441fd4989a3c",
    versioning = "6f0f4df760fc490f9d41be79f3635459",
    `variable-folders-alt` = "44cd8b9e53f544108227c78c6fb66aa9"
)

useFixtureDataset <- function(name, cmd) {
    ds_id <- fixture_ds_cw[name]
    if (!is.na(ds_id)) {
        ds <- loadDataset(ds_id, project = NULL)
        warning(paste0("MY USER EMAIL: ", email(me())))
        warning(paste0("editor_name -", ds@body$current_editor_name))
        # ds <- unlock(ds) # --- Make sure that captain@crunch.io doesn't retain editorship
        # ds <- refresh(ds)
        # warning(paste0("AFTER LOCKING: editor_name -", ds@body$current_editor_name))
        return(ds)
    }
    force(cmd)
}


# Make compressed fixtures available to downstream packages
decompress_fixtures()
