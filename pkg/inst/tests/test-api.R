context("API calling")

test_that("HTTP verbs are validated", {
    expect_identical(httr:::POST, selectHttpFunction("POST"))
    expect_error(selectHttpFunction())
    expect_error(selectHttpFunction(2))
    expect_error(selectHttpFunction(list(foo=4)))
    expect_error(selectHttpFunction("BREW"))
    expect_error(selectHttpFunction(c("PUT", "POST")))
})

if (!run.only.local.tests) {
    test_that("API root can be fetched", {
        login(test.user)
        expect_false(is.error(try(getAPIroot())))
        urls <- getAPIroot()
        expect_true(is.shoji(urls))
    })

    if (crunchAPIcanBeReached()) {
        test_that("API calls throw an error if user is not authenticated", {
            logout()
            expect_error(getAPIroot(), "403")
        })
    }

    test_that("crunchConfig", {
        expect_identical(crunchConfig(), crunchHTTPheaders())
        login(test.user)
        expect_identical(length(crunchConfig()), 2L)
    })
}