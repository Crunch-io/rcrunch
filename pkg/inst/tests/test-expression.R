context("Expressions")

with(fake.HTTP, {
    session_store$datasets <- DatasetCatalog(GET("api/datasets.json"))
    ds <- loadDataset("test ds")
    
    test_that("Arithmetic generates expressions", {
        e1 <- try(ds$birthyr + 5)
        expect_true(inherits(e1, "CrunchExpression"))
        zexp <- list(`function`="+",
            args=list(
                list(variable="birthyr"),
                list(value=5, type=list(`function`="typeof",
                    args=list(list(variable="birthyr"))))
            )
        )
        expect_identical(zcl(e1), zexp)
        e2 <- try(5 + ds$birthyr)
        expect_true(inherits(e2, "CrunchExpression"))
    })
    
    test_that("Logic generates expressions", {
        e1 <- try(ds$birthyr < 0)
        expect_true(inherits(e1, "CrunchExpression"))
    })
})

if (!run.only.local.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            test_that("Arithmetic expressions evaluate", {
                e1 <- try(ds$v3 + 5)
                expect_true(inherits(e1, "CrunchExpression"))
                e2 <- try(5 + ds$v3)
                expect_true(inherits(e2, "CrunchExpression"))
                expect_identical(as.vector(e1), as.vector(ds$v3) + 5)
                skip(expect_identical(as.vector(e1), as.vector(e2)),
                    "non-broadcastable output operand with shape (1) doesn't match the broadcast shape (20)")
                expect_identical(as.vector(ds$v3 * ds$v3), df$v3^2)
            })

            test_that("Logical expressions evaluate", {
                e1 <- try(ds$v3 < 10)
                expect_true(inherits(e1, "CrunchExpression"))
                skip(expect_identical(as.vector(e1), as.vector(ds$v3) < 10),
                    "select with logical expression not supported")
            })
            
            test_that("expressions on expresssions evaluate", {
                e3 <- try(ds$v3 + ds$v3 + 10)
                expect_true(inherits(e3, "CrunchExpression"))
                expect_identical(as.vector(e3), 2*df$v3 + 10)
                e4 <- try(ds$v3 + ds$v3 * 2)
                expect_true(inherits(e4, "CrunchExpression"))
                expect_identical(as.vector(e4), 3*df$v3)
            })
            
            test_that("filters on variables evaluate", {
                e5 <- try(ds$v3[ds$v3 < 10])
                expect_true(inherits(e5, "CrunchExpression"))
                expect_identical(as.vector(e5), c(8, 9))
            })
        })
    })
}