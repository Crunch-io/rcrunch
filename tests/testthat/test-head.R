context("head and tail")

with_test_authentication({
    ds <- newDatasetFromFixture("apidocs")
    dfhead <- head(ds)
    test_that("head on dataset", {
        expect_identical(dim(dfhead), c(6L, ncol(ds)))
        expect_identical(dfhead$q1, as.vector(ds$q1[1:6]))
    })

    test_that("head on CrunchDataFrame", {
        expect_identical(head(as.data.frame(ds)), dfhead)
    })

    test_that("head on subset of Dataset", {
        
    })

    test_that("head on Variable", {

    })

    test_that("head on CrunchExpr", {

    })

    test_that("head on Variable subset", {

    })

    ## Same for tail
})
