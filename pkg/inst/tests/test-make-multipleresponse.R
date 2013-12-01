context("Multiple response")

if (!run.only.local.tests) {
    with(test.authentication, {
        test_that("can make MR in several ways", {
            with(test.dataset(mrdf), {
                testdf <- .setup
                var <- makeMR(testdf[1:3], name="test1")
                expect_true(is.Multiple(var))
                testdf <- refresh(testdf)
                expect_equal(c("test1", "v4"), names(testdf))
            })
            with(test.dataset(mrdf), {
                testdf <- .setup
                var <- makeMR(testdf[c("mr_1", "mr_2", "mr_3")], name="test1")
                expect_true(is.Multiple(var))
                testdf <- refresh(testdf)
                expect_equal(c("test1", "v4"), names(testdf))                
            })
            with(test.dataset(mrdf), {
                testdf <- .setup
                var <- makeMR(pattern="mr_[123]", dataset=testdf, name="test1")
                expect_true(is.Multiple(var))
                testdf <- refresh(testdf)
                expect_equal(c("test1", "v4"), names(testdf))
            })
        })
        
        with(test.dataset(mrdf), {
            testdf <- .setup
            test_that("makeMR error conditions", {
                no.name <- "Must provide the name for the new variable"
                no.match <- "Pattern did not match any variables"
                need.variables <- "Invalid list of Variables to combine"
                ds.mismatch <- "`list_of_variables` must be from `dataset`"
                expect_error(makeMR(), no.name)
                expect_error(makeMR(pattern="mr_[123]", dataset=testdf), no.name)
                expect_error(makeMR(pattern="rm_", dataset=testdf, name="foo"),
                    no.match)
                expect_error(makeMR(c("mr_1", "mr_2", "mr_3"), dataset=testdf,
                    name="foo"), need.variables)
                skip(with(test.dataset(df), {
                    nottestdf <- .setup
                    expect_error(makeMR(testdf[1:3], dataset=nottestdf, name="test1"),
                        ds.mismatch)
                }))
            })
        })
    })
}

## then add tests for MR variable entities. but note that response names aren't exposed in API yet.