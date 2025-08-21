context("Automation")

test_that("string_is_file_like behaves", {
    expect_true(string_is_file_like("test.txt"))
    expect_true(string_is_file_like("test.crunch"))
    expect_false(string_is_file_like("RENAME v1 TO age;\nSET EXCLUSION v1 > 21;"))
    expect_false(string_is_file_like("test1.txt\ntest2.txt"))
    expect_false(string_is_file_like("test"))
})

with_mock_crunch({
    ds <- cachedLoadDataset("test ds") # 1 successful script
    ds2 <- cachedLoadDataset("ECON.sav") # no successful scripts

    script_text_vector <- c(
        "RENAME starttime TO interviewtime; ",
        "SET EXCLUSION birthyr > 2000;"
    )

    script_text <- paste(script_text_vector, collapse = "\n")

    temp <- tempfile(fileext = ".txt")
    writeLines(script_text, temp)

    # httptest converts the "\n" to "\\n" in its capturing
    script_text_from_request <- gsub("\n", "\\\\n", script_text)

    test_that("Query shape is right when coming from string", {
        expect_POST(
            fixed = TRUE,
            runCrunchAutomation(ds, script_text),
            "https://app.crunch.io/api/datasets/1/scripts/",
            '{"element":"shoji:entity",',
            '"body":{"body":"', script_text_from_request, '","strict_subvariable_syntax":true}}'
        )

        # Make sure this doesn't fail when there's no error
        expect_message(showScriptErrors(), NA)
        # or when reset
        reset_automation_error_env()
        expect_message(showScriptErrors(), NA)
    })

    test_that("Query shape is right when coming from string (with argument named: dataset)", {
        # Previously, runCrunchAutomation only worked on Crunch datasets,
        # so its first argument was called dataset;
        # For backwards compatibility, we want this to still work
        expect_POST(
            fixed = TRUE,
            suppressWarnings(
                runCrunchAutomation(dataset = ds, script_text, foo = 1, bar = 2, strict_subvariable_syntax = FALSE)
            ),
            "https://app.crunch.io/api/datasets/1/scripts/",
            '{"element":"shoji:entity",',
            '"body":{"body":"', script_text_from_request,
            '","foo":1,"bar":2,"strict_subvariable_syntax":false}}'
        )

        # Make sure this doesn't fail when there's no error
        expect_message(showScriptErrors(), NA)
        # or when reset
        reset_automation_error_env()
        expect_message(showScriptErrors(), NA)
    })

    test_that("Query shape is right when coming from a file", {
        expect_POST(
            runCrunchAutomation(ds, temp),
            "https://app.crunch.io/api/datasets/1/scripts/",
            '{"element":"shoji:entity",',
            '"body":{"body":"', script_text_from_request, '","strict_subvariable_syntax":true}}'
        )
    })

    test_that("Query shape is right when overriding the is_file argument", {
        expect_POST(
            runCrunchAutomation(ds, "file.txt", is_file = FALSE),
            "https://app.crunch.io/api/datasets/1/scripts/",
            '{"element":"shoji:entity",',
            '"body":{"body":"file.txt","strict_subvariable_syntax":true}}'
        )
    })

    test_that("Query shape is right when coming from vector", {
        expect_POST(
            fixed = TRUE,
            runCrunchAutomation(ds, script_text_vector),
            "https://app.crunch.io/api/datasets/1/scripts/",
            '{"element":"shoji:entity",',
            '"body":{"body":"', script_text_from_request, '","strict_subvariable_syntax":true}}'
        )
    })

    test_that("Can retrieve a non-empty scripts catalog", {
        ds_scripts <- scripts(ds)
        expect_is(ds_scripts, "ScriptCatalog")
        # On catalog
        expect_equal(
            timestamps(ds_scripts),
            as.POSIXlt("2020-05-06 17:36:27.237 UTC", tz = "UTC")
        )
        # On single script
        expect_is(ds_scripts[[1]], "Script")
        expect_true(is.script(ds_scripts[[1]]))
        expect_equal(
            timestamps(ds_scripts[[1]]),
            as.POSIXlt("2020-05-06 17:36:27.237 UTC", tz = "UTC")
        )
        expect_equal(scriptBody(ds_scripts[[1]]), script_text)
    })

    test_that("Scripts catalog print method", {
        # Better error message for problem in R 4.1 and macos-arm64
        formatted <- formatScriptCatalog(
            scripts(ds),
            from = strptime("2020-05-09", "%Y-%m-%d", tz = "UTC"),
            body_width = 10
        )
        expect_equal(formatted$Timestamp, "2 days ago")

        # full test
        expect_identical(
            formatted,
            data.frame(
                Timestamp = c("2 days ago"),
                mutations = TRUE,
                items_created = 0,
                scriptBody = paste0(strtrim(script_text, 7), "..."),
                stringsAsFactors = FALSE
            )
        )
    })

    test_that("query shape - script savepoint version", {
        expect_GET(
            scriptSavepoint(scripts(ds)[[1]]),
            "https://app.crunch.io/api/datasets/1/savepoints/v2/"
        )
    })

    test_that("query shape - undoScript", {
        expect_DELETE(
            undoScript(ds, 1),
            "https://app.crunch.io/api/datasets/1/scripts/3cb2fb/output/"
        )
        expect_DELETE(
            undoScript(ds, scripts(ds)[[1]]),
            "https://app.crunch.io/api/datasets/1/scripts/3cb2fb/output/"
        )
    })

    test_that("query shape - revertScript", {
        expect_POST(
            revertScript(ds, 1),
            "https://app.crunch.io/api/datasets/1/scripts/3cb2fb/revert"
        )
        expect_POST(
            revertScript(ds, scripts(ds)[[1]]),
            "https://app.crunch.io/api/datasets/1/scripts/3cb2fb/revert"
        )
    })

    test_that("Can interpret information from 1 script failure", {
        expect_error(
            runCrunchAutomation(ds2, "RENAME wrong_var_name TO age;"),
            "Crunch Automation Error"
        )


        expect_message(
            failures <- showScriptErrors(),
            "- \\(line 1\\) Variables wrong_var_name don't exist in the specified source"
        )

        expect_equal(
            failures,
            list(
                last_attempted_script = "RENAME wrong_var_name TO age;",
                file = NULL,
                errors = data.frame(
                    column = NA,
                    command = 1L,
                    line = 1L,
                    message = "Variables wrong_var_name don't exist in the specified source",
                    stringsAsFactors = FALSE
                ),
                script = "RENAME wrong_var_name TO age;"
            )
        )
    })

    test_that("Can interpret information from 2 script failures", {
        expect_error(
            runCrunchAutomation(
                ds2,
                "RENAME wrong_var_name TO age;\nRENAME wrong_var_name2 TO age;"
            ),
            "Crunch Automation Error"
        )


        expect_message(
            failures <- showScriptErrors(),
            paste(
                "\\(line 1\\) Variables wrong_var_name don't exist in the specified source.+",
                "\\(line 2\\) Variables wrong_var_name2 don't exist in the specified source",
                collapse = ""
            )
        )

        expect_equal(
            failures,
            list(
                last_attempted_script = paste0(
                    "RENAME wrong_var_name TO age;\nRENAME wrong_var_name2 TO age;"
                ),
                file = NULL,
                errors = data.frame(
                    column = c(NA, NA),
                    command = c(1L, 2L),
                    line = c(1L, 2L),
                    message = c(
                        "Variables wrong_var_name don't exist in the specified source",
                        "Variables wrong_var_name2 don't exist in the specified source"
                    ),
                    stringsAsFactors = FALSE
                ),
                script = "RENAME wrong_var_name TO age;\nRENAME wrong_var_name2 TO age;"
            )
        )
    })

    test_that("Can interpret error from async script failures (with automation hints)", {
        # Override progress with a pre-generated JSON at the url
        with_mocked_bindings(
            crPOST = function(..., progress.handler = NULL) {
                capture.output(crunch::pollProgress(
                    "https://app.crunch.io/api/progress-failed-async-script.json",
                    wait = 0.01,
                    error_handler = progress.handler
                ))
            }, {
                expect_error(
                    ds <- runCrunchAutomation(ds, "NOT A COMMAND"),
                    "Crunch Automation Error"
                )
            }
        )

        expect_message(
            failures <- showScriptErrors(),
            "\\(line 1\\) Invalid command: NOT",
        )
    })

    test_that("Can interpret error from async script failures (without automation hints)", {
        # Override progress with a pre-generated JSON at the url
        with_mocked_bindings(
            crPOST = function(..., progress.handler = NULL) {
                capture.output(crunch::pollProgress(
                    "https://app.crunch.io/api/progress-failed-async-script-msg.json",
                    wait = 0.01,
                    error_handler = progress.handler
                ))
            }, {
                expect_error(
                    expect_message(
                        ds <- runCrunchAutomation(ds, 'CREATE CATEGORICAL CONSTANT abc LABEL "a";'),
                        "Task failed unexpectedly"
                    ),
                    "Variable with alias: abc already exists"
                )
            }
        )
    })

    test_that("Get message on success when dry_run is TRUE", {
        with_POST(
            "", # Don't actually need to load anything, just need no POST error
            expect_message(
                runCrunchAutomation(ds, "# no commands", dry_run = TRUE),
                "Script dry run was successful"
            )
        )

    })

    test_that("error truncation works", {
        expected <- " - (line 1) Error 1\n - (line 2) Error 2\n - ... (Showing first 2 of 3 errors)"
        attr(expected, "truncated") <- TRUE

        expect_equal(
            automation_errors_text(
                data.frame(
                    column = NA,
                    command = 1:3,
                    line = 1:3,
                    message = c("Error 1", "Error 2", "Error 3")
                ),
                2
            ),
            expected
        )
    })

    test_that("folder-level operation fails on root", {

        root_project_folder <- projects()
        script <- "CREATE FOLDER 'My not-to-be folder';"

        expect_error(
            runCrunchAutomation(root_project_folder, script),
            "not support Crunch Automation scripts"
        )
    })

    test_that("folder-level operation  works with string script", {

        project_folder   <- cd(projects(), 'Project One')
        script           <- "CREATE FOLDER 'My to-be folder';"
        expected_url     <- "https://app.crunch.io/api/projects/project1/execute/"
        expected_body    <- paste0(
            '{"element":"shoji:view",',
            paste0('"value":', '"', script, '"'), '}'
        )

        expect_POST(
            runCrunchAutomation(project_folder, script),
            expected_url, expected_body,
            fixed = TRUE
        )
    })

    test_that("extra arguments result in an error for folder-level operations", {

        project_folder   <- cd(projects(), 'Project One')
        script           <- "CREATE FOLDER 'My to-be folder';"
        expected_url     <- "https://app.crunch.io/api/projects/project1/execute/"
        expected_body    <- paste0(
            '{"element":"shoji:view",',
            paste0('"value":', '"', script, '"'), '}'
        )

        expect_error(
            expect_POST(
                runCrunchAutomation(project_folder, script, foo = 1, bar = '2'),
                expected_url, expected_body,
                fixed = TRUE
            ),
            'not supported'
        )
    })

    test_that("folder-level operation works with character vector (length > 1) script", {

        project_folder   <- cd(projects(), 'Project One')
        script           <- c(
            "CREATE FOLDER 'My to-be folder';",
            "CREATE FOLDER 'Another folder';"
        )
        expected_url     <- "https://app.crunch.io/api/projects/project1/execute/"
        # # httptest converts the '\n' to '\\n' in its capturing, that's why
        # there is '\\n' instead of '\n' below
        expected_body    <- paste0(
            '{"element":"shoji:view",',
            paste0('"value":', '"', paste(script, collapse = '\\n'), '"'), '}'
        )

        expect_POST(
            runCrunchAutomation(project_folder, script),
            expected_url, expected_body,
            fixed = TRUE
        )
    })

    test_that("folder-level operation works with file script", {

        project_folder   <- cd(projects(), 'Project One')
        script           <- "CREATE FOLDER 'My to-be folder';"
        temp             <- tempfile(fileext = ".txt")
        writeLines(script, temp)

        expected_url     <- "https://app.crunch.io/api/projects/project1/execute/"
        expected_body    <- paste0(
            '{"element":"shoji:view",',
            paste0('"value":', '"', paste(script, collapse = '\n'), '"'), '}'
        )

        expect_POST(
            runCrunchAutomation(project_folder, temp),
            expected_url, expected_body,
            fixed = TRUE
        )
    })

})

with_test_authentication({
    skip("Automation can sometimes time out, so skip for now")
    ds <- newDatasetFromFixture("apidocs")

    test_that("We can run a simple automation script and then undo it", {
        ds <- runCrunchAutomation(
            ds,
            'CREATE LOGICAL q1 == "Cat" OR q1 == "Dog" AS mammal;'
        )

        expect_identical(
            names(categories(ds$mammal)),
            c("Selected", "Other", "No Data")
        )
        expect_equivalent(
            as.array(crtabs(~q1, data = ds)),
            array(c(6, 4, 3),
                  dim = 3,
                  dimnames = list(q1 = c("Cat", "Dog", "Bird"))
            )
        )
        expect_equivalent(
            as.array(crtabs(~mammal, data = ds)),
            array(c(10, 3),
                  dim = 2,
                  dimnames = list(combined_pets = c("Selected", "Other"))
            )
        )

        ds <- undoScript(ds, scripts(ds)[[1]])
        expect_false("mammal" %in% names(ds))
    })

    test_that("Failures work okay", {
        expect_error(
            ds <- runCrunchAutomation(ds, "FAKE CRUNCH AUTOMATION COMMAND"),
            "Crunch Automation Error"
        )
        expect_message(errors <- showScriptErrors())
        expect_is(errors$errors, "data.frame")
        expect_equal(names(errors$errors), c("column", "command", "line", "message"))
        expect_true(!is.na(errors$errors$message))
    })

    test_that("We can revert a simple script", {
        ds <- runCrunchAutomation(
            ds,
            'CREATE LOGICAL q1 == "Cat" AS cat_lgl;'
        )
        expect_true("cat_lgl" %in% names(ds))
        ds <- revertScript(ds, scripts(ds)[[1]])
        expect_false("cat_lgl" %in% names(ds))
    })
})
