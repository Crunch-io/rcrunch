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
    as_data_frame_mr = "1b840ac37e0e44dcae9351a1f94464cc",
    `case-variables` = "92ae349f74694fffa9f64af447118472",
    `case-when` = "433dfd832bca406a95bd47e36a41aba6",
    `change-categories` = "4f820236661b4915a83c0d9ff070c59a",
    `change-categories-array` = "8e5b6a8f55d14b97a370e99cff7349fa",
    `combine-categories` = "15ee58e989634d60b79d83bd25370e02",
    `combine-responses` = "66530412683a4e2e8a571dc67736e215",
    `copy-variable` = "58308aff611541b8936b6d7308de478f",
    `cube-collapse-dimensions` = "1f6b7b9f7707464fa7678a9133472d69",
    `cube-dims` = "65aa0c7c031d42a887b910f389f458a6",
    `cube-errors` = "4a49b26956d74d9ba0fd9a6ef38c7c4c",
    `cube-index-table` = "573b12edf5374db9ad70c1f4a2927ccf",
    `cube-mr` = "35ed584501c84b59b5633f4568001ebe",
    `cube-nway` = "8ce0e1339823483f9a769d2366bc9050",
    `cube-with-array` = "81d06be8aaca4575a2cf4c3775925afc",
    `cubes-main` = "6af4b2dc55a0409c8969e797d4779014",
    `cubes-scorecard` = "c4c352627e314bf6ad0462354785bc5c",
    cut = "93200670cff0432fba9133b80efc4b50",
    `deep-copy1` = "a9683b0410b5477a81d50f645c36ee8d",
    `deep-copy2` = "ffd432ec6e0e429ab2dcc6593e5dc864",
    derive = "8a15d092a93f42b799dc4ae7dd45cbba",
    `derive-array1` = "978b2f0d748741a984e96e72c6bd5b22",
    `derive-array2` = "3590b1ec57b844e38fdb00477276895b",
    `derive-array3` = "6a2282c84343401f8d91afe516fb35e9",
    `derive-debug` = "55a4c357c3584e4dae9f1b8c55592583",
    `drop-rows1` = "f710bdd80b25453fbcecf37fed9b6922",
    `drop-rows2` = "492f222294d146e3b14442e151efd511",
    `exclusion-filter` = "84589eafeb834330908d0b60ae438b32",
    `export-dataset` = "c0dd5418513d48e0b6d798cece5ffcb3",
    expressions = "39574ec56510400cb0bb2b3e0eb95f3a",
    filters = "8fc1daae280e4d4d9b7903ae229b81a7",
    `flip-array` = "b9d5cb2b40854635adbd54a797d75715",
    folders = "3422726896c94ff5ac697af6489a6753",
    fork1 = "dd73a450afba451fb4eb23bdb370530d",
    fork2 = "2cd4c21ca4ed4d34b6bf23f2d1fdcd67",
    `hide-variables` = "f052ec08cb524e979fdfc9c8f72c0a31",
    `hide-variables-array` = "4b8b4b18bb66471fb37d6c5f19d174e0",
    `hide-variables-single` = "eb1bc446b9c54e2a9d49e944a94baab4",
    interact = "db009cad526d4dd99c2870c8fcff6bf2",
    lock = "2c3136a6245c4d3c95fdd28fc2131eba",
    `merge-datasets1` = "9a41a56355324b999f11caa9f7ae0d0b",
    `merge-datasets3` = "6e4d9512a4b54ee3bd4b3943190d6d4a",
    `merge-datasets4` = "a0b38b6abce24bcf83788293c6840e93",
    `merge-datasets5` = "fbdc7362ca904387885fcf06964d2620",
    `merge-datasets6` = "9d23d1a7663349cfa4270a90a0620571",
    shoji = "74f6761a78ed43f89298803198f514e8",
    subvariables = "e022af112642437cb6caa595103b3c33",
    update = "b26a0ea3aaab4c339f5208380123cb58",
    `update-array1` = "6847b26b6630403a9c069cced38c822f",
    `update-array2` = "5283ac77b05345099eec2ebc18e2fd59",
    `update-errors` = "bf05bf93f3c84bd39e11679de83567f8",
    utftesting = "0b493aafa64e4449aafd748de9849801",
    `variable-as-methods` = "a2fdf2c9369f4483a7cc6f87671db45a",
    `variable-catalog` = "0afe5e09f98042e9b86e10a3044df59b",
    `variable-delete` = "5d6f70a08c3444d6af6a4ec486ceccde",
    `variable-delete-array` = "0cba915d9a004874adbb765741b59d44",
    `variable-folders` = "e7ad29ab0e54465f80d0608879ac4992",
    `variable-folders-alt` = "ec017c7f4ec0437a9fb42ecad2ecd46b",
    `variable-metadata` = "3da1cab1e97f483aa10a53e2699274ce",
    `variable-order` = "cd0f3624021b45a68faf4d0f2fdef073",
    `variable-transforms` = "8062e6ece8da447f832bde90e604143c",
    `variable-type` = "ef9e56a59ef74e199df3e2ce0c5d0b97",
    versioning = "f78a3c0a067d43eeaa57548f99258b81",
    `cube-transforms` = "15f38dde34ca444c8baff0ac192c6e30",
    `cube-with-expression` = "85ffb95d92a746a49e10cb4277c1dfec"
)

useFixtureDataset <- function(name, cmd) {
    ds_id <- fixture_ds_cw[name]
    if (!is.na(ds_id)) {
        ds <- loadDataset(ds_id, project = NULL)
        warning(paste0("BEFORE LOCKING: editor_name -", ds@body$current_editor_name))
        ds <- lock(ds) # --- Make sure that captain@crunch.io doesn't retain editorship
        ds <- refresh(ds)
        warning(paste0("AFTER LOCKING: editor_name -", ds@body$current_editor_name))
        return(ds)
    }
    force(cmd)
}


# Make compressed fixtures available to downstream packages
decompress_fixtures()
