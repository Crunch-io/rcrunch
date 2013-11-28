context("Multiple response")

if (!run.only.local.tests) {
    with(test.authentication, {
        with(test.dataset(mrdf), {
            testdf <- .setup
            test_that("can make MR", {
                var <- makeMR(testdf[1:3], name="test1")
                ## need to fix [ method for Dataset to work for character (names). not just @.Data
                expect_true(is.Multiple(var))
                expect_true("test1" %in% names(refresh(testdf)))
                
                ## test other ways of specifying
                
                ## test error conditions
                
                ## then add tests for MR variable entities. but note that response names aren't exposed in API yet.
            })
        })
    })
}