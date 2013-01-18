context("User stuff")

test_that("User URLs can be fetched", {
    login("***REMOVED***")
    expect_false(is.error(getUserURLs()))
    urls <- getUserURLs()
    expect_true(is.list(urls))
})