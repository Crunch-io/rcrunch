context("Derive a new variable")

# TODO: mock a derivation for unit testing

with_test_authentication({
    ds <- newDataset(df)
    try(ds$v3a <- ds$v3 + 5)
    test_that("A derived variable is created on the server", {
        expect_true("v3a" %in% names(allVariables(refresh(ds))))
    })
    test_that("The derived variable has been added to the hierarchical order", {
        expect_true("v3a" %in% names(variables(refresh(ds))))
        expect_true("v3a" %in% names(variables(ds)))
        expect_true(is.variable(ds$v3a))
    })
    test_that("Can derive a numeric from a numeric", {
        expect_true(is.Numeric(ds$v3a))
        expect_identical(as.vector(ds$v3a), as.vector(ds$v3) + 5)
    })

    test_that("derivation pulls the derivation", {
        expect_is(derivation(ds$v3a), "CrunchExpr")
        expect_fixed_output(derivation(ds$v3a), "Crunch expression: v3 + 5")
    })

    test_that("derivation returns NULL if the variable is not derived", {
        expect_null(derivation(ds$v3))
    })

    test_that("derivation()<- input validation ", {
        expect_error(derivation(ds$v3) <- ds$v3 + 1,
                     paste0("The variable ", dQuote("v3"),
                            " must already be a derived variable."))
        try(ds$v3c <- ds$v3 + 5)
        expect_true(is.derived(ds$v3c))
        expect_error(derivation(ds$v3c) <- "not an expression",
                     paste0("The value ", dQuote("not an expression"),
                            " must be a CrunchExpr, got a character instead."))
    })

    ## Now update v3's values and confirm that v3a is still linked
    try(ds$v3 <- df$v3 + 7)
    test_that("The source variable was in fact updated", {
        expect_equivalent(as.vector(ds$v3), df$v3 + 7)
    })
    test_that("A derived numeric is updated when its source variable is modified", {
        expect_identical(as.vector(ds$v3a), as.vector(ds$v3) + 5)
        expect_equivalent(as.vector(ds$v3a), df$v3 + 12)
    })
    test_that("What happens when you try to derive a logical", {
        expect_error(ds$v3b <- ds$v3 < 15,
            "Cannot currently derive a logical variable")
    })
})
