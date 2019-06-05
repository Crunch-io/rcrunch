context("Cube error handling")

with_mock_crunch({
    ds <- loadDataset("test ds")

    test_that("'formula' must be provided", {
        expect_error(crtabs(), "Must provide a formula")
    })

    test_that("formula must be a valid formula", {
        expect_error(
            crtabs("asdf", data = ds),
            paste0(dQuote("formula"), " is not a valid formula")
        )
    })

    test_that("formula '.' argument is not permitted", {
        expect_error(
            crtabs(~., data = ds),
            paste("Crunch formulae do not support", dQuote("."), "in formula")
        )
    })

    test_that("formula must have variables", {
        expect_error(
            crtabs("~", data = ds),
            paste0(dQuote("formula"), " is not a valid formula")
        )
        expect_error(
            crtabs(~1, data = ds),
            "Must supply one or more variables"
        )
    })

    test_that("'data' must be a Dataset", {
        expect_error(crtabs(~a), paste(dQuote("data"), "must be a Dataset"))
        ## Support a case of data=missing, i.e. eval formula as is?
        expect_error(
            crtabs(~a, data = NULL),
            paste(dQuote("data"), "must be a Dataset")
        )
    })

    test_that("Reserved function names cannot be variable aliases", {
        expect_error(
            crtabs(~ mean + bin(birthyr), data = ds),
            paste0(
                "Cannot evaluate a cube with reserved name: ",
                dQuote("mean")
            )
        )
        expect_error(
            crtabs(~ mean + sd, data = ds),
            paste0(
                "Cannot evaluate a cube with reserved names: ",
                dQuote("mean"), " and ", dQuote("sd")
            )
        )
    })

    test_that("What happens if 'measures' are not aggregations?", {
        expect_error(
            crtabs(gender ~ birthyr, data = ds),
            "Left side of formula must be a valid aggregation"
        )
        expect_error(
            crtabs(birthyr ~ gender, data = ds),
            "Left side of formula must be a valid aggregation"
        )
    })

    test_that("What if there are aggregations on the RHS?", {
        expect_error(
            crtabs(~ mean(birthyr), data = ds),
            "Right side of formula cannot contain aggregation functions"
        )
    })

    test_that("'rollup' on non-Datetime", {
        expect_error(
            crtabs(~ rollup(gender), data = ds),
            paste0(
                "Cannot rollup a variable of type ",
                dQuote("categorical")
            )
        )
    })

    test_that("'as_array' on non-MR", {
        expect_error(
            crtabs(~ as_array(gender), data = ds),
            paste0(
                "Cannot analyze a variable of type ",
                dQuote("categorical"), " 'as_array'"
            )
        )
    })

    test_that("Unsupported aggregation functions", {
        expect_error(
            crtabs(cumsum(birthyr) ~ gender, data = ds),
            "no method for coercing this S4 class to a vector"
        )
        ## This is standard R behavior, not special handling.
        ## Just for illustration of what will happen.
    })

    test_that("Invalid filter argument in 'data'", {
        expect_error(crtabs(~gender, data = ds[ds$NOTAVARIABLE == 3, ]),
            "Invalid expression: ds$NOTAVARIABLE == 3",
            fixed = TRUE
        )
        expect_error(crtabs(~gender, data = ds[ds$gender %in% "Male" | ds$NOTAVARIABLE == 3, ]),
            paste0(
                "Invalid expression (probably a reference to a variable that ",
                "doesn't exist): ds$gender %in% \"Male\" | ds$NOTAVARIABLE == 3"
            ),
            fixed = TRUE
        )
    })

    test_that("can't request NULL as subvariable (bad subvar ref)", {
        expect_error(crtabs(~ catarray$subvar2 + catarray$NOTAVAR, data = ds),
            "Invalid cube dimension: catarray$NOTAVAR cannot be NULL",
            fixed = TRUE
        )
        expect_error(crtabs(~ catarray$foo + catarray$subvar2 + catarray$NOTAVAR, data = ds),
            "Invalid cube dimensions: catarray$foo and catarray$NOTAVAR cannot be NULL",
            fixed = TRUE
        )
    })
})

with_test_authentication({
    ds <- newDataset(df[, 1:4])
    test_that("All variables must be present in data", {
        expect_error(
            crtabs(~ aaa + v3, data = ds),
            "object 'aaa' not found"
        )
        ## But works if variable is in workspace
        aaa <- ds$v4
        skip(paste0(
            "Appears not to work in the test at least. aaa is in the enclos ",
            "environment but it doesn't find it"
        ))
        expect_equivalent(
            as.array(crtabs(~ aaa + bin(v3), data = ds)),
            array(c(1, 1, 3, 2, 2, 3, 3, 2, 1, 2),
                dim = c(2L, 5L),
                dimnames = list(
                    v4 = c("B", "C"),
                    v3 = c("5-10", "10-15", "15-20", "20-25", "25-30")
                )
            )
        )
        ## What about a combination of a Variable and a local R vector?
    })

    test_that("Reserved function names cannot be variable aliases", {
        dsb <- ds
        al <- aliases(dsb@variables)
        dsb@variables@index[[which(al == "v1")]]$alias <- "mean"
        dsb@variables@index[[which(al == "v2")]]$alias <- "sd"

        expect_identical(names(dsb), c("mean", "sd", "v3", "v4"))
        expect_error(
            crtabs(~ mean + bin(v3), data = dsb),
            paste0(
                "Cannot evaluate a cube with reserved name: ",
                dQuote("mean")
            )
        )
        ## But you can still get analyses with other variables
        expect_equivalent(
            as.array(crtabs(~ bin(v3), data = dsb)),
            array(c(2, 5, 5, 5, 3),
                dim = c(5L),
                dimnames = list(v3 = c("5-10", "10-15", "15-20", "20-25", "25-30"))
            )
        )
    })

    test_that("What happens if there are more than one vars on LHS?", {
        out <- crtabs(list(mean(v3), sd(v3)) ~ v4, data = ds)
        ## Actually, this works. Make some assertions to see that it's right
    })

    test_that("Limit on number of dimension variables", {
        out <- crtabs(~ v1 + v2 + v3 + v4, data = ds)
        ## nope, 4 works too. Assert about it
    })

    test_that("prop.table cannot take margin greater than dim", {
        expect_error(
            prop.table(crtabs(~ v4 + v3, data = ds), margin = 3),
            "Margin 3 exceeds Cube's number of dimensions \\(2\\)"
        )
    })
})
