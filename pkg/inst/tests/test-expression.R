context("Expressions")

with(fake.HTTP, {
    session_store$datasets <- DatasetCatalog(GET("api/datasets.json"))
    ds <- loadDataset("test ds")
    
    test_that("Arithmetic generates expressions", {
        e1 <- try(ds$birthyr + 5)
        expect_true(inherits(e1, "CrunchExpression"))
        e2 <- try(5 + ds$birthyr)
        expect_true(inherits(e2, "CrunchExpression"))
    })
    
    test_that("Logic generates expressions", {
        e1 <- try(ds$birthyr < 0)
        expect_true(inherits(e1, "CrunchExpression"))
    })
}

if (!run.only.local.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            ds <- .setup
            
            test_that("Arithmetic expressions evaluate", {
                e1 <- try(ds$v3 + 5)
                expect_true(inherits(e1, "CrunchExpression"))
                e2 <- try(5 + ds$v3)
                expect_true(inherits(e2, "CrunchExpression"))
                expect_identical(as.vector(e1), as.vector(ds$v3) + 5)
                expect_identical(as.vector(e1), as.vector(e2))
            })

            test_that("Logical expressions evaluate", {
                e1 <- try(ds$v3 < 10)
                expect_true(inherits(e1, "CrunchExpression"))
                expect_identical(as.vector(e1), as.vector(ds$v3) < 10)
            })
        })
    })
}