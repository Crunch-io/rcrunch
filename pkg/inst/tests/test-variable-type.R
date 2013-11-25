context("Variable types")

v <- lapply(vars, as.variable)

test_that("Variable type method", {
    expect_identical(type(v[["age"]]), "numeric")
    expect_identical(type(v$gender), "categorical")
})

test_that("R object types are translated to Crunch vars", {
    # replace this with some testing of preUpload and postUpload
})

if (!run.only.local.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            test_that("type casting and 'as'", {
                testdf <- .setup 
                v1.was.text <- is.Text(testdf[["v1"]])
                if (!v1.was.text) type(testdf[["v1"]]) <- "text"
                testvar <- testdf[["v1"]]
    
                expect_true(is.Text(testvar))
                expect_true(is.Numeric(castVariable(testvar, "numeric")))
                expect_true(is.Text(castVariable(testvar, "text")))    
                type(testvar) <- "numeric"
                expect_true(is.Numeric(testvar))
                expect_false(is.Numeric(testdf[["v1"]])) 
                expect_true(is.Numeric(refresh(testdf[["v1"]])))
                    ## since they're the same remote object
                if (v1.was.text) type(testdf[["v1"]]) <- "text"
                    ## to reset the dataset, since there aren't test teardowns
        
                expect_error(castVariable(, "foo"), 
                    paste(sQuote("foo"), "is not a valid Crunch variable type."))
            })
        })
    })
}