context("Variables")

with_mock_HTTP({
    ds <- loadDataset("test ds")

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
    })

    test_that("Variable setters (mock)", {
        tp <- tuple(ds$gender)@body
        tp$name <- "Sex"
        mock.tuple <- structure(list(tp["name"]), .Names=self(ds$gender))
        expect_PATCH(name(ds$gender) <- "Sex",
            self(variables(ds)),
            toJSON(mock.tuple))
    })

    test_that("Variable setters don't hit server if data not changed", {
        expect_no_request(name(ds$gender) <- "Gender")
    })

    test_that("refresh", {
        expect_identical(ds$gender, refresh(ds$gender))
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
    })

    name(ds$v1) <- "Variable 1"
    description(ds$v2) <- "Description 2"
    alias(ds$v1) <- "var1"
    test_that("can modify name, description, alias on var in dataset", {
        expect_null(ds$v1)
        expect_identical(name(ds$var1), "Variable 1")
        expect_identical(alias(ds$var1), "var1")
        expect_identical(description(ds$v2), "Description 2")
        ds <- refresh(ds)
        expect_null(ds$v1)
        expect_identical(name(ds$var1), "Variable 1")
        expect_identical(description(ds$v2), "Description 2")
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
})
