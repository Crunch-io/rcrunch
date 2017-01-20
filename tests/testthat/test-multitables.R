context("Multitables")

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

    test_that("newMultitable", {
        skip("TODO")
        expect_POST(newMultitable(~ gender + mymrset, data=ds),
            'api/datasets/1/multitables/',
            '{}')
    })
})
