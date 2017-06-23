context("Variables")

with_mock_crunch({
    ds <- loadDataset("test ds")
    ds2 <- loadDataset("an archived dataset", "archived")

    test_that("Variable init, as, is", {
        expect_true(is.variable(ds[[1]]))
        expect_true(all(vapply(ds, is.variable, logical(1))))
        expect_false(is.variable(5))
        expect_false(is.variable(NULL))
    })

    test_that("Variable subclass definitions, is", {
        expect_true(is.dataset(ds))
        expect_true(is.Categorical(ds$gender))
        expect_true(is.Numeric(ds$birthyr))
        expect_true(is.Text(ds[["textVar"]]))
        expect_true(is.Datetime(ds$starttime))
        expect_true(is.Multiple(ds$mymrset))
        expect_true(is.Array(ds$mymrset))
        expect_false(is.CA(ds$mymrset))
    })

    test_that("is.derived", {
        expect_false(is.derived(ds2$gender))
        expect_false(is.derived(ds2$birthyr))
        expect_true(is.derived(ds2$mymrset2))
    })

    test_that("has.categories", {
        expect_true(has.categories(ds$gender))
        expect_true(has.categories(ds$mymrset))
        expect_false(has.categories(ds$birthyr))
        expect_true(has.categories("categorical_array"))
        expect_identical(has.categories(c("categorical", "numeric", "text",
            "datetime", "categorical_array", "multiple_response")),
            c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE))
    })

    test_that("Categories for categorical", {
        thisone <- categories(ds$gender)
        expect_true(is.categories(thisone))
        expect_length(thisone, 3)
        expect_true(is.category(thisone[[1]]))
    })
    test_that("Categories for noncategorical", {
        expect_null(categories(ds$birthyr))
    })

    test_that("Variable metadata retrieved from tuples", {
        expect_identical(name(ds$gender), "Gender")
        expect_identical(description(ds$starttime), "Interview Start Time")
        expect_identical(alias(ds$gender), "gender")
        expect_identical(notes(ds$gender), "")
        expect_identical(notes(ds$birthyr), "Asked instead of age")
    })

    test_that("Variable setter requests", {
        expect_PATCH(name(ds$gender) <- "Sex",
            "https://app.crunch.io/api/datasets/1/variables/",
            '{"https://app.crunch.io/api/datasets/1/variables/gender/":{"name":"Sex"}}')
        expect_PATCH(notes(ds$gender) <- "extra info",
            "https://app.crunch.io/api/datasets/1/variables/",
            '{"https://app.crunch.io/api/datasets/1/variables/gender/":{"notes":"extra info"}}')
    })

    test_that("Variable setters don't hit server if data not changed", {
        expect_no_request(name(ds$gender) <- "Gender")
    })

    test_that("Name setter requires non-missing character input", {
        expect_error(name(ds$gender) <- 45,
            'Names must be of class "character"')
        expect_error(name(ds$gender) <- NA_character_,
            "Names must be non-missing")
    })

    test_that("Cannot unset name or alias", {
        expect_error(name(ds$gender) <- NULL,
            'Names must be of class "character"')
        expect_error(alias(ds$gender) <- NULL,
            'Names must be of class "character"')
    })

    test_that("Backstop method for if you try to set name on NULL", {
        expect_error(name(ds$NOTAVARIABLE) <- "Not a variable",
            "Cannot set name on NULL")
    })

    test_that("refresh", {
        expect_identical(ds$gender, refresh(ds$gender))
    })

    test_that("can modify digits on var object", {
        expect_identical(digits(ds$birthyr), 2L)
        expect_PATCH(digits(ds$birthyr) <- 0,
                     "https://app.crunch.io/api/datasets/1/variables/birthyr/",
                     '{"element":"shoji:entity","body":{"format":{"data":',
                     '{"digits":0}}}}')

        expect_error(digits(ds$birthyr) <- -1, "digit specifications should be between 0 and 16")
        expect_error(digits(ds$birthyr) <- 999, "digit specifications should be between 0 and 16")
        expect_error(digits(ds$birthyr) <- 0.7, "digit specifications should be an integer")
        expect_error(digits(ds$birthyr) <- "0.7", "digit specifications should be an integer")
    })
})

with_test_authentication({
    ds <- newDataset(df)

    test_that("show methods", {
        expect_identical(getShowContent(ds$v3), c(
            "v3 (numeric)",
            "",
            "   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. ",
            "   8.00   12.75   17.50   17.50   22.25   27.00 "
        ))
        expect_identical(getShowContent(ds$v4), c(
                "v4 (categorical)",
                "",
                "  Count",
                "B    10",
                "C    10"
            ))
        ## TODO: add other types. And move to fixtures.
    })

    test_that("before modifying", {
        expect_identical(name(ds$v1), "v1")
        expect_identical(description(ds$v2), "")
        expect_identical(alias(ds$v1), "v1")
        expect_identical(name(ds$v3), "v3")
        expect_identical(alias(ds$v3), "v3")
        expect_identical(notes(ds$v1), "")
    })

    name(ds$v1) <- "Variable 1"
    description(ds$v2) <- "Description 2"
    notes(ds$v1) <- "Some additional information"
    alias(ds$v1) <- "var1"
    test_that("can modify name, description, alias, notes on var in dataset", {
        expect_null(ds$v1)
        expect_identical(name(ds$var1), "Variable 1")
        expect_identical(alias(ds$var1), "var1")
        expect_identical(description(ds$v2), "Description 2")
        expect_identical(notes(ds$var1), "Some additional information")
        ds <- refresh(ds)
        expect_null(ds$v1)
        expect_identical(name(ds$var1), "Variable 1")
        expect_identical(notes(ds$var1), "Some additional information")
        expect_identical(description(ds$v2), "Description 2")
    })

    test_that("Can unset description and notes", {
        description(ds$v2) <- NULL
        expect_identical(description(ds$v2), "")
        notes(ds$var1) <- NULL
        expect_identical(notes(ds$var1), "")
    })

    v3 <- ds$v3
    name(v3) <- "alt"
    description(v3) <- "asdf"
    alias(v3) <- "Alias!"
    test_that("can modify name, description, alias on var object", {
        expect_identical(name(v3), "alt")
        expect_identical(description(v3), "asdf")
        expect_identical(alias(v3), "Alias!")
        v3 <- refresh(v3)
        expect_identical(name(v3), "alt")
        expect_identical(description(v3), "asdf")
        expect_identical(alias(v3), "Alias!")
    })

    test_that("can modify digits on var object", {
        expect_identical(digits(ds$v3), 2L)
        expect_silent(digits(ds$v3) <- 0)
        expect_identical(digits(ds$v3), 0L)
    })
})
