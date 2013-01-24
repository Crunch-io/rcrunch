context("User stuff")

test_that("User URLs can be fetched", {
    login(test.user)
    expect_false(is.error(getUserURLs()))
    urls <- getUserURLs()
    expect_true(is.list(urls))
})

test_that("User URLs cannot be fetched if logged out", {
    logout()
    expect_error(getUserURLs(), 
        "You must authenticate before making this request")
})