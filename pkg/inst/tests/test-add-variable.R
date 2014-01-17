context("Add a variable to a dataset")

test_that("toVariable parses R data types", {
    expect_identical(toVariable(2L:4L), list(values=2L:4L, type="numeric"))
    expect_identical(toVariable(letters[1:3]), list(values=c("a", "b", "c"), type="text"))
})

if (!run.only.local.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            test_that("addVariable creates a new remote variable", {
                testdf <- .setup
                testdf <- addVariable(testdf, df$v3, name="New var")
                expect_true("newVar" %in% names(testdf))
                nv <- testdf$newVar
                expect_true(is.Numeric(nv))
                expect_true(is.Numeric(testdf[['v3']]))
                expect_identical(as.vector(nv), as.vector(testdf$v3))
            })
        })
    })
}