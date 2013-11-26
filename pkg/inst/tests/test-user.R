context("User stuff")

if (!run.only.local.tests) {
    test_that("User URLs can be fetched", {
        login()
            expect_false(is.error(getUserResourceURLs()))
            urls <- getUserResourceURLs()
            expect_true(is.list(urls))
        logout()
    })

    test_that("User URLs cannot be fetched if logged out", {
        logout()
        expect_error(getUserResourceURLs(), 
            "You must authenticate before making this request")
    })
}
