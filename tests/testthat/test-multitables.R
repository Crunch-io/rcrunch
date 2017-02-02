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
        ## Note that this PATCHes the entity, not the catalog
        expect_PATCH(names(mults)[2] <- "New name",
            'api/datasets/1/multitables/4de322/',
            '{"name":"New name"}')
    })
    test_that("Multitable catalog is.public", {
        expect_identical(is.public(mults), c(FALSE, TRUE))
        ## Note that this PATCHes the entity, not the catalog
        expect_PATCH(is.public(mults)[2] <- FALSE,
            'api/datasets/1/multitables/4de322/',
            '{"is_public":false}')
        expect_no_request(is.public(mults)[2] <- TRUE)
    })

    test_that("Multitable object methods", {
        m <- mults[[1]]
        expect_identical(name(m), "My banner")
        expect_PATCH(name(m) <- "Another name",
            'api/datasets/1/multitables/ed30c4/',
            '{"name":"Another name"}')
        expect_PATCH(is.public(m) <- TRUE,
            'api/datasets/1/multitables/ed30c4/',
            '{"is_public":true}')
        expect_no_request(is.public(m) <- FALSE)
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

    mult <- multitables(ds)[["allpets + q1"]]
    test_that("Can make the multitable entity public/personal", {
        expect_false(is.public(mult))
        is.public(mult) <- TRUE
        expect_true(is.public(refresh(mult)))
        is.public(mult) <- FALSE
        expect_false(is.public(refresh(mult)))
    })

    test_that("Can make the multitable public/personal on the catalog", {
        expect_false(is.public(multitables(ds))[1])
        is.public(multitables(ds))[1] <- TRUE
        expect_true(is.public(refresh(multitables(ds)))[1])
        is.public(multitables(ds))[1] <- FALSE
        expect_false(is.public(refresh(multitables(ds)))[1])
    })

    test_that("Can edit the multitable name", {
        expect_identical(name(mult), "allpets + q1")
        name(mult) <- "A new name"
        expect_identical(name(mult), "A new name")
        expect_identical(name(refresh(mult)), "A new name")
        expect_identical(names(refresh(multitables(ds))), "A new name")
        names(multitables(ds)) <- "Yet another name"
        expect_identical(names(multitables(ds)), "Yet another name")
        expect_identical(names(refresh(multitables(ds))), "Yet another name")
    })
})
