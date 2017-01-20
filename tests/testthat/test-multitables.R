context("Multitables")

test_that("default name for formula", {
    expect_identical(formulaRHS(a+b~c+d+rollup(e)), "c + d + rollup(e)")
    expect_identical(formulaRHS("a+b~c+d+rollup(e)"), "c+d+rollup(e)")
})

with_mock_HTTP({
    ds <- loadDataset("test ds")   ## Has 2 multitables
    ds2 <- loadDataset("ECON.sav") ## Has no multitables

    test_that("multitables() getter", {
        expect_is(multitables(ds), "MultitableCatalog")
        expect_is(multitables(ds2), "MultitableCatalog")
        expect_length(multitables(ds), 2)
        expect_length(multitables(ds2), 0)
    })

    test_that("Extract multitable", {
        expect_is(multitables(ds)[[2]], "Multitable")
        expect_is(multitables(ds)[["Shared multitable"]], "Multitable")
        expect_error(multitables(ds)[[99]], "subscript out of bounds: 99")
        expect_null(multitables(ds)[["NOTVALID"]])
    })

    mults <- multitables(ds)
    test_that("Multitable catalog names", {
        expect_identical(names(mults), c("My banner", "Shared multitable"))
        expect_PATCH(names(mults)[2] <- "New name",
            'api/datasets/1/multitables/',
            '{"api/datasets/1/multitables/4de322/":{"name":"New name"}}')
    })
    test_that("Multitable catalog is.public", {
        expect_identical(is.public(mults), c(FALSE, TRUE))
        ## TODO: check that this PATCH is allowed or whether you have to patch the entity
        expect_PATCH(is.public(mults)[2] <- FALSE,
            'api/datasets/1/multitables/',
            '{"api/datasets/1/multitables/4de322/":{"is_public":false}}')
        expect_no_request(is.public(mults)[2] <- TRUE)
    })

    test_that("Multitable object methods", {
        m <- mults[[1]]
        expect_identical(name(m), "My banner")
    })

    test_that("newMultitable", {
        expect_POST(newMultitable(~ gender + mymrset, data=ds, name="New multitable"),
            'api/datasets/1/multitables/',
            '{"element":"shoji:entity","body":{',
            '"name":"New multitable",',
            '"template":[{"query":[{"variable":"api/datasets/1/variables/gender/"}],',
            '"variable":"api/datasets/1/variables/gender/"},',
            '{"query":[{"function":"selected_array",',
            '"args":[{"variable":"api/datasets/1/variables/mymrset/"}]},',
            '{"each":"api/datasets/1/variables/mymrset/"}],',
            '"variable":"api/datasets/1/variables/mymrset/"}]',
            '}}')
        with_POST("api/datasets/1/multitables/4de322/", {
            m <- newMultitable(~ gender + mymrset, data=ds, name="New multitable")
            expect_is(m, "Multitable")
        })
    })

    test_that("newMultitable provides a default name based on the formula", {
        expect_POST(newMultitable(~ gender + mymrset, data=ds),
            'api/datasets/1/multitables/',
            '{"element":"shoji:entity","body":{',
            '"name":"gender + mymrset",',
            '"template":[{"query":[{"variable":"api/datasets/1/variables/gender/"}],',
            '"variable":"api/datasets/1/variables/gender/"},',
            '{"query":[{"function":"selected_array",',
            '"args":[{"variable":"api/datasets/1/variables/mymrset/"}]},',
            '{"each":"api/datasets/1/variables/mymrset/"}],',
            '"variable":"api/datasets/1/variables/mymrset/"}]',
            '}}')
        with_POST("api/datasets/1/multitables/4de322/", {
            m <- newMultitable(~ gender + mymrset, data=ds, name="New multitable")
            expect_is(m, "Multitable")
        })
    })

    test_that("newMultitable validation", {
        expect_error(newMultitable(), "Must provide a formula")
        expect_error(newMultitable(.),
            paste(dQuote("data"), "must be a Dataset"))
        expect_error(newMultitable(., data=""),
            paste(dQuote("data"), "must be a Dataset"))
    })
})

with_test_authentication({
    ds <- newDatasetFromFixture("apidocs")
    test_that("Multitable catalog", {
        expect_is(multitables(ds), "MultitableCatalog")
        expect_length(multitables(ds), 0)
    })

    test_that("Can make a multitable", {
        m <- newMultitable(~ allpets + q1, data=ds)
        expect_identical(name(m), "allpets + q1")
    })
})
