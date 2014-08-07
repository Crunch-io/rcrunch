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
    expect_true(all(config.args %in% names(crunchConfig())))
    expect_identical(crunchConfig()$encoding, "gzip")
})

test_that("crunchUserAgent", {
    expect_true(grepl("rcrunch", getCrunchUserAgent()))
    expect_true(grepl("rcrunch", session_store$.globals$user_agent))
    expect_false(is.error(try(setCrunchUserAgent("anotherpackage/3.1.4"))))
    expect_true(grepl("anotherpackage", getCrunchUserAgent()))
})

if (run.integration.tests) {
    with(test.authentication, 
        test_that("API root can be fetched", {
            expect_false(is.error(try(getAPIroot())))
            urls <- getAPIroot()
            expect_true(is.shoji(urls))
        }))

    if (crunchAPIcanBeReached()) {
        test_that("API calls throw an error if user is not authenticated", {
            logout()
            expect_error(getAPIroot(), "401")
        })
    }

    with(test.authentication, 
        test_that("cookie is in the request header", {
            expect_true("cookie" %in% names(crunchConfig()))
    }))
    
    test_that("Deprecated endpoints tell user to upgrade", {
        expect_error(GET("http://httpbin.org/status/410"), 
            "The API resource at http://httpbin.org/status/410 has moved permanently. Please upgrade rcrunch to the latest version.")
    })
}