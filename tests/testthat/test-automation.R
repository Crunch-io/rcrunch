context("Automation")

test_that("string_is_file_like behaves", {
    expect_true(string_is_file_like("test.txt"))
    expect_true(string_is_file_like("test.crunch"))
    expect_false(string_is_file_like("RENAME v1 TO age;\nSET EXCLUSION v1 > 21;"))
    expect_false(string_is_file_like("test1.txt\ntest2.txt"))
    expect_false(string_is_file_like("test"))
})

with_mock_crunch({
    ds <- loadDataset("test ds")

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

    test_that("Can get script checkpoint version", {
        expect_GET(
            scriptCheckpointVersion(scripts(ds)[[1]]),
            "https://app.crunch.io/api/datasets/1/savepoints/v2/"
        )
    })


    ds2 <- loadDataset("ECON.sav")

    test_that("Can interpret information from a script failure", {
        expect_error(
            runCrunchAutomation(ds2, "RENAME wrong_var_name TO age;"),
            "Crunch Automation Error. Run `crunchAutomationFailure()"
        )

        expect_equal(
            crunchAutomationFailure(),
            list(
                errors = data.frame(
                    message = "Variables wrong_var_name don't exist in the specified source",
                    command = 1L,
                    line = 1,
                    stringsAsFactors = FALSE
                ),
                script = "RENAME wrong_var_name TO age;"
            )
        )
    })
})

# TODO: Integration tests?
# with_test_authentication({
#     whereas("", {
#     })
# })