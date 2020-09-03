context("Derive a new variable")

with_mock_crunch({
    ds <- loadDataset("test ds")
    # manually make a variable from json mock
    birthyrPlus <- CrunchVariable(
        VariableTuple(
            index_url = "https://app.crunch.io/api/datasets/1/variables/?relative=on",
            entity_url = "https://app.crunch.io/api/datasets/1/variables/birthyrPlus/",
            body = crGET("https://app.crunch.io/api/datasets/1/variables/birthyrPlus/")$body
        )
    )

    test_that("derivation pulls the derivation", {
        expect_is(derivation(birthyrPlus), "CrunchExpr")
        expect_true(is.derived(birthyrPlus))
        expect_prints(derivation(birthyrPlus), "Crunch expression: birthyr + 100")
    })

    test_that("derivation returns NULL if the variable is not derived", {
        expect_null(derivation(ds$birthyr))
    })

    test_that("derivation()<- input validation", {
        expect_error(
            derivation(ds$birthyr) <- ds$birthyr + 1,
            paste0(
                "The variable ", dQuote("Birth Year"),
                " must already be a derived variable."
            )
        )
        expect_error(
            derivation(birthyrPlus) <- "not an expression",
            paste0(
                "The value ", dQuote("not an expression"),
                " must be a CrunchExpr, got a character instead."
            )
        )
    })

    test_that("derivation()<- issues a PATCH", {
        expect_PATCH(
            derivation(birthyrPlus) <- ds$birthyr + 1,
            "https://app.crunch.io/api/datasets/1/variables/birthyrPlus/",
            '{"derivation":{"function":"+","args":[{"variable":"https://app.',
            'crunch.io/api/datasets/1/variables/birthyr/"},{"value":1}]}}'
        )
    })

    test_that("derivation()<-NULL and is.derived()<-FALSE issues a PATCH", {
        expect_PATCH(
            derivation(birthyrPlus) <- NULL,
            "https://app.crunch.io/api/datasets/1/variables/birthyrPlus/",
            '{"derived":false}'
        )
        expect_PATCH(
            is.derived(birthyrPlus) <- FALSE,
            "https://app.crunch.io/api/datasets/1/variables/birthyrPlus/",
            '{"derived":false}'
        )
        expect_no_request(is.derived(birthyrPlus) <- TRUE)
        expect_no_request(is.derived(ds$birthyr) <- FALSE)
        expect_no_request(derivation(ds$birthyr) <- NULL)
    })

    test_that("derivation()<-NULL with a non-derived variable does not error.", {
        expect_no_request(derivation(ds$birthyr) <- NULL)
    })

    test_that("is.derived([not derived])<-TRUE errors.", {
        expect_error(
            is.derived(ds$birthyr) <- TRUE,
            "can't change a non-derived variable into a derived one with is.derived()."
        )
    })

    test_that("derive from logical expression issues correct POST", {
        expect_POST(
            ds$kids <- ds$birthyr > 2000,
            "https://app.crunch.io/api/datasets/1/variables/",
            '{"derivation":{"function":">","args":[{"variable":',
            '"https://app.crunch.io/api/datasets/1/variables/birthyr/"},',
            '{"value":2000}]},"name":"kids","alias":"kids"}'
        )
    })
})

with_test_authentication({
    ds <- newDataset(df)
    ds$v3a <- ds$v3 + 5
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
        expect_prints(derivation(ds$v3a), "Crunch expression: v3 + 5")
    })

    test_that("derivation returns NULL if the variable is not derived", {
        expect_null(derivation(ds$v3))
    })

    ds$v3l <- ds$v3 > 10
    test_that("can derive a logical expression", {
        expect_true(is.derived(ds$v3l))
        expect_true(is.Categorical(ds$v3l))
        expect_prints(derivation(ds$v3l), "Crunch expression: v3 > 10")
        expect_equivalent(as.vector(ds$v3l), df$v3 > 10)
    })

    test_that("derivation()<- can change a derived variable", {
        ds$v3d <- ds$v3 + 10
        expect_true(is.derived(ds$v3d))
        expect_prints(derivation(ds$v3d), "Crunch expression: v3 + 10")
        derivation(ds$v3d) <- ds$v3 + 1
        expect_true(is.derived(ds$v3d))
        expect_prints(derivation(ds$v3d), "Crunch expression: v3 + 1")
    })

    ## Now update v3's values and confirm that v3a is still linked
    ds$v3 <- df$v3 + 7
    test_that("The source variable was in fact updated", {
        expect_equivalent(as.vector(ds$v3), df$v3 + 7)
    })

    test_that("A derived numeric is updated when its source variable is modified", {
        expect_identical(as.vector(ds$v3a), as.vector(ds$v3) + 5)
        expect_equivalent(as.vector(ds$v3a), df$v3 + 12)
    })

    test_that("derivation()<-NULL and is.derived()<-FALSE removes derivations and instantiates", {
        # refresh to catch derivations made in previous tests
        ds <- refresh(ds)
        expect_true(is.derived(ds$v3a))
        expect_silent(derivation(ds$v3a) <- NULL)
        expect_false(is.derived(ds$v3a))
        expect_null(derivation(ds$v3a))
        expect_equal(
            as.vector(ds$v3a),
            c(20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39)
        )

        expect_silent(is.derived(ds$v3d) <- FALSE)
        expect_false(is.derived(ds$v3d))
        expect_null(derivation(ds$v3d))
        expect_equal(
            as.vector(ds$v3d),
            c(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35)
        )
    })
})
