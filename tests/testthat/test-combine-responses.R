context("Combine responses")

# expect_error(ds$combined_mr <- combine(ds$mymrset, name="MR combined",
#     list(list(name="Extremes", responses=c("First", "Last")))),
#     mr.payload, fixed=TRUE)
with_mock_crunch({
    ds <- cachedLoadDataset("test ds")

    extremes <- VariableDefinition(
        name = "MR combined",
        description = "Please select all that apply",
        discarded = FALSE,
        notes = "",
        format = list(summary = list(digits = 2), data = list(digits = 2)),
        view = list(
            include_noneoftheabove = FALSE,
            column_width = NULL
        ),
        derivation = list(
            `function` = "combine_responses",
            args = list(
                list(variable = "https://app.crunch.io/api/datasets/1/variables/mymrset/"),
                list(value = list(
                    list(
                        name = "Extremes",
                        combined_ids = I(c(
                            "https://app.crunch.io/api/datasets/1/variables/mymrset/subvariables/subvar2/", # nolint
                            "https://app.crunch.io/api/datasets/1/variables/mymrset/subvariables/subvar3/" # nolint
                        ))
                    ),
                    list(
                        name = "Second",
                        combined_ids = I(c("https://app.crunch.io/api/datasets/1/variables/mymrset/subvariables/subvar1/")) # nolint
                    )
                ))
            )
        )
    )
    test_that("combine() constructs the correct VarDef for MR", {
        combine.resps <- combine(ds$mymrset,
            name = "MR combined",
            list(list(name = "Extremes", responses = c("First", "Last")))
        )
        expect_json_equivalent(combine.resps, extremes)
        expect_json_equivalent(combine.resps$expr, extremes$expr)
    })
    test_that("combineResponses() is an alias for combine()", {
        combine.resps <- combineResponses(ds$mymrset,
            name = "MR combined",
            list(list(name = "Extremes", responses = c("First", "Last")))
        )
        expect_json_equivalent(combine.resps, extremes)
    })

    test_that("Default name for combine responses", {
        def <- combine(
            ds$mymrset,
            list(list(name = "Extremes", responses = c("First", "Last")))
        )
        expect_identical(def$name, "mymrset (2 responses)")
    })

    test_that("Validation for 'combinations'", {
        expect_error(
            combine(ds$mymrset,
                name = "MR combined",
                list(list(name = "Extremes", categories = c("First", "Last")))
            ),
            "'combinations' must be a list of combination specifications"
        )
        expect_error(
            combine(ds$mymrset,
                name = "MR combined",
                list(list(name = "Extremes", responses = c("First", "Not a response")))
            ),
            paste(
                "Response", dQuote("Extremes"),
                "does not reference valid subvariables"
            )
        )
        expect_error(
            combine(ds$mymrset,
                name = "MR combined",
                list(list(name = "Extremes", responses = c(1, 2)))
            ),
            "Combinations must reference 'responses' by name or alias"
        )
        expect_error(
            combine(ds$mymrset,
                name = "MR combined",
                list(list(name = "Second", responses = c("First", "Last")))
            ),
            paste("Duplicate response name given:", dQuote("Second"))
        )
    })
})

with_test_authentication({
    with(test.dataset(newDatasetFromFixture("apidocs")), {
        test_that("We can create a new MR by combining", {
            ds$combined_allpets <- combine(ds$allpets,
                name = "All pets (combined)",
                list(list(name = "Mammals", responses = c("Cat", "Dog")))
            )
            expect_identical(
                names(subvariables(ds$combined_allpets)),
                c("Mammals", "Bird")
            )
        })
    })
})
