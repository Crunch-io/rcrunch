context("API calling")

test_that("HTTP verbs are validated", {
    expect_identical(httr:::POST, selectHttpFunction("POST"))
    expect_error(selectHttpFunction())
    expect_error(selectHttpFunction(2))
    expect_error(selectHttpFunction(list(foo=4)))
    expect_error(selectHttpFunction("BREW"))
    expect_error(selectHttpFunction(c("PUT", "POST")))
})

config.args <- c("sslversion", "httpheader", "verbose")

test_that("crunchConfig has right structure", {
    expect_true(setequal(names(crunchConfig()), config.args))
})

if (!run.only.local.tests) {
    test_that("API root can be fetched", {
        login()
            expect_false(is.error(try(getAPIroot())))
            urls <- getAPIroot()
            expect_true(is.shoji(urls))
        logout()
    })

    if (crunchAPIcanBeReached()) {
        test_that("API calls throw an error if user is not authenticated", {
            logout()
            expect_error(getAPIroot(), "401")
        })
    }

    test_that("crunchConfig", {
        login()
            expect_identical(length(crunchConfig()), length(config.args) + 1L)
        logout()
    })
}