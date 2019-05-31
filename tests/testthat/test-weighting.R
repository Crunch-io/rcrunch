context("Weights")

with_mock_crunch({
    oldds <- loadDataset("test ds")
    newds <- loadDataset("ECON.sav")

    test_that("Getting weights", {
        expect_null(weight(oldds))
        expect_warning(
            expect_identical(weight(newds), newds$birthyr),
            "Variable birthyr is hidden"
        )
    })
    
    test_that("Can get weights from a subset dataset", {
        expect_warning(
            expect_identical(weight(newds[c("gender", "starttime")]), newds$birthyr),
            "Variable birthyr is hidden"
        )
    })

    test_that("Setting weights", {
        expect_PATCH(
            weight(oldds) <- oldds$birthyr,
            "https://app.crunch.io/api/datasets/1/preferences/",
            '{"weight":"https://app.crunch.io/api/datasets/1/variables/birthyr/"}'
        )
        expect_PATCH(
            weight(newds) <- NULL,
            "https://app.crunch.io/api/datasets/3/preferences/",
            '{"weight":null}'
        )
    })
    test_that("is.weight assignment method", {
        expect_PATCH(
            is.weight(oldds$birthyr) <- TRUE,
            "https://app.crunch.io/api/datasets/1/preferences/",
            '{"weight":"https://app.crunch.io/api/datasets/1/variables/birthyr/"}'
        )
        expect_silent(is.weight(oldds$birthyr) <- FALSE)
    })
    test_that("is.weight returns false when the variable is not a weight", {
        expect_false(is.weight("character"))
        expect_false(is.weight(mtcars))
        expect_false(is.weight(NULL))
    })
    test_that("is weight assignment errors correctly", {
        expect_error(is.weight(oldds$birthyr) <- "char",
            "is.TRUEorFALSE(value) is not TRUE",
            fixed = TRUE
        )
        expect_error(is.weight(oldds$birthyr) <- c(TRUE, FALSE),
            "is.TRUEorFALSE(value) is not TRUE",
            fixed = TRUE
        )
    })

    test_that("No request is made to set a weight that already is your weight", {
        expect_no_request(weight(oldds) <- NULL)
        expect_warning(
            expect_no_request(weight(newds) <- newds$birthyr),
            "Variable birthyr is hidden"
        )
    })

    test_that("Errors are properly handled when setting weight", {
        expect_error(
            weight(newds) <- "a",
            "Weight must be a Variable or NULL"
        )
        ## test error handling when trying to set non-numeric (need backend?)
    })

    test_that("weightVariables method", {
        expect_identical(weightVariables(newds), "birthyr")
        with(temp.option(crunch.namekey.dataset = "name"), {
            expect_identical(weightVariables(newds), "Birth Year")
        })
        expect_identical(weightVariables(oldds), c())
    })
    test_that("weightVariables can be assigned", {
        body <- '{"element":"shoji:order","self":"https://app.crunch.io/api/datasets/1/variables/weights/","description":"Order of the weight variables for this dataset","graph":["https://app.crunch.io/api/datasets/1/variables/birthyr/"]} '
        expect_PUT(
            weightVariables(oldds) <- oldds$birthyr,
            "https://app.crunch.io/api/datasets/1/variables/weights/",
            body
        )
        expect_PUT(
            is.weightVariable(oldds$birthyr) <- TRUE,
            "https://app.crunch.io/api/datasets/1/variables/weights/",
            body
        )
    })
    test_that("is.weightVariable", {
        expect_false(is.weightVariable(oldds$birthyr))
    })
    test_that("is.weightVariable assignment method", {
        body <- '{"element":"shoji:order","self":"https://app.crunch.io/api/datasets/1/variables/weights/","description":"Order of the weight variables for this dataset","graph":["https://app.crunch.io/api/datasets/1/variables/birthyr/"]} '
        expect_PUT(
            is.weightVariable(oldds$birthyr) <- TRUE,
            "https://app.crunch.io/api/datasets/1/variables/weights/",
            body
        )
    })

    test_that("assigning incorrect entries to weightVariables errors correctly", {
        expect_error(
            weightVariables(oldds) <- "notvar",
            "notvar is not a numeric Crunch variable."
        )
        expect_error(
            weightVariables(oldds) <- oldds$gender,
            "Gender is not a numeric Crunch variable."
        )
        expect_error(
            weightVariables(oldds) <- list(oldds$gender, oldds$birthyr),
            "Gender is not a numeric Crunch variable."
        )
        expect_error(
            weightVariables(oldds) <- list(oldds$gender, oldds$textVar),
            "Gender and Text variable ftw are not numeric Crunch variables."
        )
    })

    test_that("generateWeightEntry errors correctly", {
        expect_error(generateWeightEntry("bad_formula"),
            paste0(
                dQuote("bad_formula"),
                " is not a valid formula. Use the form ds$var ~ c(50, 20, 30)"
            ),
            fixed = TRUE
        )
        expect_error(generateWeightEntry(object),
            paste0(
                dQuote("object"),
                " is not a valid formula. Use the form ds$var ~ c(50, 20, 30)"
            ),
            fixed = TRUE
        )
        expect_error(generateWeightEntry(oldds$birthyr ~ c(30, 30, 40)),
            "oldds$birthyr is not a categorical Crunch variable",
            fixed = TRUE
        )
        expect_error(generateWeightEntry(oldds$gender ~ c(10, 10, 10, 10, 10, 50)),
            "Number of targets does not match number of categories for oldds$gender",
            fixed = TRUE
        )
        expect_error(generateWeightEntry(oldds$gender ~ c(30, 20, 30)),
            "Targets do not add up to 100% for oldds$gender",
            fixed = TRUE
        )
        expect_error(generateWeightEntry(oldds$gender ~ c("a", "b", "c")),
            "Targets are not numeric for oldds$gender",
            fixed = TRUE
        )
        expect_error(generateWeightEntry(oldds$gender ~ c(50, 50, NA)),
            paste0(
                dQuote("oldds$gender ~ c(50, 50, NA)"),
                " contains NA values"
            ),
            fixed = TRUE
        )
    })
    expected_weight_definition <- list(
        name = "weight",
        derivation = list(
            `function` = "rake",
            args = list(
                list(
                    variable = "https://app.crunch.io/api/datasets/1/variables/gender/",
                    targets = list(c(1, 0.4), c(2, 0.6), c(-1, 0))
                )
            )
        )
    )
    expected_attribute_definition <- list(
        alias = "test_alias",
        name = "weight",
        derivation = list(
            `function` = "rake",
            args = list(
                list(
                    variable = "https://app.crunch.io/api/datasets/1/variables/gender/",
                    targets = list(c(1, 0.4), c(2, 0.6), c(-1, 0))
                )
            )
        )
    )

    test_that("makeWeight generates the expected VariableDefinition", {
        expect_equivalent(
            makeWeight(oldds$gender ~ c(40, 60, 0), name = "weight"),
            expected_weight_definition
        )
    })
    test_that("makeWeight allows decimal target input", {
        expect_equivalent(
            makeWeight(oldds$gender ~ c(.4, .6, 0), name = "weight"),
            expected_weight_definition
        )
    })
    test_that("You can provide two targets to a variable with three categories", {
        expect_equivalent(
            makeWeight(oldds$gender ~ c(50, 50), name = "weight"),
            makeWeight(oldds$gender ~ c(.5, .5, 0), name = "weight")
        )
    })
    test_that("makeWeight allows user to specify variable definition attributes", {
        expect_equivalent(
            makeWeight(oldds$gender ~ c(40, 60, 0),
                name = "weight", alias = "test_alias"
            ),
            expected_attribute_definition
        )
    })
})

with_test_authentication({
    with(test.dataset(df), {
        test_that("Can set weight variable", {
            expect_null(weight(ds))
            weight(ds) <- ds$v3
            expect_equivalent(weight(ds), ds$v3)
            ds <- refresh(ds)
            expect_equivalent(weight(ds), ds$v3)
            weight(ds) <- NULL
            expect_null(weight(ds))
        })

        test_that("If weight is set, computations are weighted", {
            expect_equivalent(
                table(ds$v4),
                structure(c(B = 10, C = 10), class = "table")
            )
            weight(ds) <- ds$v3
            expect_equivalent(
                table(ds$v4),
                structure(c(B = sum(seq(8, 26, 2)), C = sum(seq(9, 27, 2))),
                    class = "table"
                )
            )
        })

        test_that("If weight is set, dim() is still unweighted", {
            weight(ds) <- NULL
            expect_identical(nrow(ds), 20L)
            weight(ds) <- ds$v3
            expect_identical(nrow(ds), 20L)
            ds <- refresh(ds)
            expect_identical(nrow(ds), 20L)
        })
    })
    with(test.dataset(df), {
        test_that("We have a clean dataset", {
            expect_null(weight(ds))
        })
        test_that("Reverting to old version rolls back weight variables", {
            ds$w <- 1:20
            weight(ds) <- ds$w
            expect_equivalent(weight(ds), ds$w)
            ds <- restoreVersion(ds, 1)
            expect_null(ds$w)
            expect_null(weight(ds))
        })
        test_that("And I can add new weights because weight_variables is valid", {
            ds <- refresh(ds)
            ds$w2 <- 2:21
            weight(ds) <- ds$w2
            expect_equivalent(weight(ds), ds$w2)
        })
    })

    with(test.dataset(df), {
        test_that("We have a clean dataset", {
            expect_null(weightVariables(ds))
        })
        ds$weight <- sample(c(.2, .8), 20, replace = TRUE)
        ds$weight22 <- sample(c(.2, .8), 20, replace = TRUE)
        ds$weight23 <- sample(c(.5, .5), 20, replace = TRUE)
        test_that("modifyWeightVariables appends, removes, and replaces", {
            modifyWeightVariables(ds, "weight")
            expect_identical(weightVariables(ds), "weight")
            modifyWeightVariables(ds, "weight22", "append")
            expect_identical(weightVariables(ds), c("weight", "weight22"))
            modifyWeightVariables(ds, "weight22", "remove")
            expect_identical(weightVariables(ds), c("weight"))
            modifyWeightVariables(ds, NULL, "replace")
            expect_identical(weightVariables(ds), NULL)
        })
        test_that("weightVariables can be assigned", {
            weightVariables(ds) <- ds$weight23
            expect_identical(
                weightVariables(ds), "weight23"
            )
            weightVariables(ds) <- list(ds$weight23, ds$weight22)
            expect_identical(
                weightVariables(ds), c("weight22", "weight23")
            )
            weightVariables(ds) <- NULL
            expect_null(weightVariables(ds))
            weightVariables(ds) <- c("weight22", "weight23")
            expect_identical(
                weightVariables(ds), c("weight22", "weight23")
            )
        })
        test_that("is.weightVariable identifies weight variable and allows it to be set", {
            expect_true(is.weightVariable(ds$weight22))
            is.weightVariable(ds$weight22) <- FALSE
            expect_false(is.weightVariable(ds$weight22))
            expect_identical(weightVariables(ds), "weight23")
            is.weightVariable(ds$weight22) <- TRUE
            expect_true(is.weightVariable(ds$weight22))
            expect_identical(weightVariables(ds), c("weight22", "weight23"))
            expect_false(is.weightVariable("notvar"))
        })
        test_that("is.weightVariable identifies weight variable and allows it to be set", {
            weight(ds) <- ds$weight22
            expect_true(is.weight(ds$weight22))
            is.weight(ds$weight22) <- FALSE
            expect_false(is.weight(ds$weight22))
            is.weight(ds$weight22) <- TRUE
            expect_identical(weight(ds), ds$weight22)
        })
    })

    with(test.dataset(df), {
        test_that("I can delete my weight variable and add a new one", {
            ds$w <- 1:20
            weight(ds) <- ds$w
            expect_equivalent(weight(ds), ds$w)
            expect_true(is.Numeric(ds$w))
            expect_equivalent(
                as.array(crtabs(~v4, data = ds)),
                array(c(100, 110), dim = 2L, dimnames = list(v4 = c("B", "C")))
            )
            ## Delete that variable. Confirm that it is gone and we are
            ## unweighted
            with(consent(), ds$w <- NULL)
            expect_null(weight(ds))
            expect_null(ds$w)
            expect_equivalent(
                as.array(crtabs(~v4, data = ds)),
                array(c(10, 10), dim = 2L, dimnames = list(v4 = c("B", "C")))
            )
            ## Now add another weight and repeat. Confirm that we can
            ## and that calculations are weighted
            ds$w <- 20:1
            weight(ds) <- ds$w
            expect_equivalent(weight(ds), ds$w)
            expect_true(is.Numeric(ds$w))
            expect_equivalent(
                as.array(crtabs(~v4, data = ds)),
                array(c(110, 100), dim = 2L, dimnames = list(v4 = c("B", "C")))
            )
        })
    })
    with(test.dataset(df), {
        expected_weights <- c(
            0.6, 1.4, 0.6, 1.4, 0.6, 1.4, 0.6, 1.4, 0.6, 1.4,
            0.6, 1.4, 0.6, 1.4, 0.6, 1.4, 0.6, 1.4, 0.6, 1.4
        )
        test_that("makeWeight returns the expected weights", {
            ds$weight <- makeWeight(ds$v4 ~ c(30, 70, 0), name = "weight")
            expect_identical(as.vector(ds$weight), expected_weights)
        })
        test_that("Assigning a VariableDefinition to weight(ds) works", {
            weight(ds) <- makeWeight(ds$v4 ~ c(30, 70, 0), name = "weight2")
            expect_identical(weight(ds), ds$weight2)
            expect_identical(as.vector(weight(ds)), expected_weights)
        })
    })
})
