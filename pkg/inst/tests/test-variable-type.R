context("Variable types")

v <- lapply(vars, as.variable)

test_that("Variable type method", {
    expect_identical(type(v[["age"]]), "numeric")
    expect_identical(type(v$gender), "categorical")
})

test_that("R object types are translated to Crunch vars", {
    expect_identical(crunchType(1:5), "numeric")
    expect_identical(crunchType(letters[1:5]), "text")
    expect_identical(crunchType(df), 
        structure(c("numeric", "text", "numeric", "categorical"),
        .Names=names(df)))
})

if (!run.only.local.tests) {
    test_that("type casting and 'as'", {
        login()
            testdf <- loadDataset("making_a_dataset_from_df") 
                ## from previous test
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
        logout()
    })
}