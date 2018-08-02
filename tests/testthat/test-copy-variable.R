context("Shallow copies of variables")

with_mock_crunch({
    ds <- loadDataset("test ds")
    test_that("copy creates a correct VariableDefinition", {
        expect_is(copy(ds$gender), "VariableDefinition")
        expected <- VariableDefinition(
            name = "Gender (copy)",
            alias = "gender_copy",
            description = "Gender",
            notes = "",
            discarded = FALSE,
            format = list(summary = list(digits = 2)),
            view = list(
                include_missing = FALSE,
                show_counts = FALSE,
                show_codes = FALSE,
                column_width = NULL
            ),
            derivation = list(
                `function` = "copy_variable",
                args = list(
                    list(variable = "https://app.crunch.io/api/datasets/1/variables/gender/")
                )
            )
        )
        expect_json_equivalent(copy(ds$gender), expected)
    })
    test_that("deep copy creates a non-derived variable", {
        expect_false(copy(ds$gender, deep = TRUE)$derived)
    })
})

with_test_authentication({
    with(test.dataset(newDatasetFromFixture("apidocs")), {
        q1_url <- self(ds$q1)
        varcat_url <- self(variables(ds))
        test_that("Can copy and manipulate a categorical variable", {
            expect_false("copy1" %in% names(ds))
            expect_true("q1" %in% names(ds))
            expect_silent(ds$copy1 <- copy(ds$q1, name = "copy1"))
            expect_identical(as.vector(ds$copy1), as.vector(ds$q1))
            expect_false(name(ds$copy1) == name(ds$q1))
            expect_false(alias(ds$copy1) == alias(ds$q1))
            expect_false(self(ds$copy1) == self(ds$q1))
            ds <- refresh(ds)
            expect_true("copy1" %in% names(ds))
            expect_true("q1" %in% names(ds))

            ## Edit category in copy
            names(categories(ds$copy1))[2] <- "Canine"
            expect_identical(
                names(categories(ds$copy1))[1:3],
                c("Cat", "Canine", "Bird")
            )
            expect_identical(
                names(categories(ds$q1))[1:3],
                c("Cat", "Dog", "Bird")
            )

            ## Edit categories in original
            categories(ds$q1)[1:2] <- categories(ds$q1)[2:1]
            expect_identical(
                names(categories(ds$copy1))[1:3],
                c("Cat", "Canine", "Bird")
            )
            expect_identical(
                names(categories(ds$q1))[1:3],
                c("Dog", "Cat", "Bird")
            )
        })

        test_that("Can copy an array variable and manipulate it independently", {
            ds$allpets2 <- copy(ds$allpets)
            expect_true("allpets" %in% names(ds))
            expect_true("allpets2" %in% names(ds))
            expect_identical(name(ds$allpets2), "All pets owned (copy)")
            name(ds$allpets2) <- "Copy of allpets"
            expect_identical(name(ds$allpets2), "Copy of allpets")
            expect_identical(name(ds$allpets), "All pets owned")

            ## Edit subvariables in the copy
            subvariables(ds$allpets2)[1:2] <- subvariables(ds$allpets2)[2:1]
            expect_identical(
                names(subvariables(ds$allpets2)),
                c("Dog", "Cat", "Bird")
            )
            expect_identical(
                names(subvariables(ds$allpets)),
                c("Cat", "Dog", "Bird")
            )

            ## Edit subvariable names in the original
            names(subvariables(ds$allpets))[2] <- "Canine"
            expect_identical(
                names(subvariables(ds$allpets2)),
                c("Dog", "Cat", "Bird")
            )
            expect_identical(
                names(subvariables(ds$allpets)),
                c("Cat", "Canine", "Bird")
            )
        })

        test_that("Can copy subvariables (as non-subvars)", {

        })
    })
})
