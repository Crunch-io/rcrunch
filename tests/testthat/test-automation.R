context("Automation")

test_that("string_is_file_like behaves", {
    expect_true(string_is_file_like("test.txt"))
    expect_true(string_is_file_like("test.crunch"))
    expect_false(string_is_file_like("RENAME v1 TO age;\nSET EXCLUSION v1 > 21;"))
    expect_false(string_is_file_like("test1.txt\ntest2.txt"))
    expect_false(string_is_file_like("test"))
})

with_mock_crunch({
    ds <- loadDataset("test ds") # 1 successful script
    ds2 <- loadDataset("ECON.sav") # no successful scripts

    script_text <- paste(
        "RENAME starttime TO interviewtime;",
        "SET EXCLUSION birthyr > 2000;",
        sep = " \n"
    )

    temp <- tempfile(fileext = ".txt")
    writeLines(script_text, temp)

    # httptest converts the "\n" to "\\n" in it's capturing
    script_text_from_request <- gsub("\n", "\\\\n", script_text)

    test_that("Query shape is right when coming from string", {
        expect_POST(
            fixed = TRUE,
            runCrunchAutomation(ds, script_text),
            "https://app.crunch.io/api/datasets/1/scripts/",
            '{"element":"shoji:entity",',
            '"body":{"body":"', script_text_from_request, '"}}'
        )

        # Make sure this doesn't fail when there's no error
        expect_message(crunchAutomationFailure(), NA)
        # or when reset
        reset_automation_error_env()
        expect_message(crunchAutomationFailure(), NA)
    })

    test_that("Query shape is right when coming from a file", {
        expect_POST(
            runCrunchAutomation(ds, temp),
            "https://app.crunch.io/api/datasets/1/scripts/",
            '{"element":"shoji:entity",',
            '"body":{"body":"', script_text_from_request, '"}}'
        )
    })

    test_that("Query shape is right when overriding the is_file argument", {
        expect_POST(
            runCrunchAutomation(ds, "file.txt", is_file = FALSE),
            "https://app.crunch.io/api/datasets/1/scripts/",
            '{"element":"shoji:entity",',
            '"body":{"body":"file.txt"}}'
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
        expect_equal(scriptBody(ds_scripts), script_text)
        # On single script
        expect_is(ds_scripts[[1]], "Script")
        expect_equal(
            timestamps(ds_scripts[[1]]),
            as.POSIXlt("2020-05-06 17:36:27.237 UTC", tz = "UTC")
        )
        expect_equal(scriptBody(ds_scripts[[1]]), script_text)
    })

    test_that("Scripts catalog print method", {
        expect_identical(
            formatScriptCatalog(
                scripts(ds),
                from = strptime("2020-05-09", "%Y-%m-%d"),
                body_width = 10
            ),
            data.frame(
                Timestamp = c("2 days ago"),
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
            failures <- crunchAutomationFailure(),
            "- \\(line 1\\) Variables wrong_var_name don't exist in the specified source"
        )

        expect_equal(
            failures,
            list(
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
            failures <- crunchAutomationFailure(),
            paste(
                "\\(line 1\\) Variables wrong_var_name don't exist in the specified source.+",
                "\\(line 2\\) Variables wrong_var_name2 don't exist in the specified source",
                collapse = ""
            )
        )

        expect_equal(
            failures,
            list(
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

    test_that("error truncation works", {
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
            " - (line 1) Error 1\n - (line 2) Error 2\n - ... (Showing first 2 of 3 errors)"
        )
    })
})

with_test_authentication({
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
        expect_message(errors <- crunchAutomationFailure())
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