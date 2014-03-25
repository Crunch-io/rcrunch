context("Variable types")

with(fake.HTTP, {
    ## Variable fake fixtures for dataset
    test.ds <- as.dataset(GET("api/datasets/dataset1.json"))
    
    test_that("Variable type method", {
        expect_identical(type(test.ds[["birthyr"]]), "numeric")
        expect_identical(type(test.ds$gender), "categorical")
    })
})

if (!run.only.local.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            test_that("type casting and 'as'", {
                testdf <- .setup 
                if (!is.Text(testdf[["v1"]])) type(testdf[["v1"]]) <- "text"
                testvar <- testdf[["v1"]]
    
                expect_true(is.Text(testvar))
                expect_true(is.Numeric(castVariable(testvar, "numeric")))
                expect_true(is.Text(castVariable(testvar, "text")))    
                type(testvar) <- "numeric"
                expect_true(is.Numeric(testvar))
                expect_true(is.Numeric(testdf[["v1"]]))
                    ## since they're the same remote object
        
                expect_error(castVariable(, "foo"), 
                    paste(sQuote("foo"), "is not a Crunch variable type that can be assigned."))
            })
        })
    })
}