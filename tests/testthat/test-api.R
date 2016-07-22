context("API calling")

test_that("Deprecated endpoints tell user to upgrade", {
    fake410 <- fakeResponse("http://crunch.io/410", status_code=410, json=list())
    expect_error(handleAPIresponse(fake410),
        paste("The API resource at http://crunch.io/410 has moved permanently.",
              "Please upgrade crunch to the latest version."))
})

with_mock_HTTP({
    test_that("crunch.debug does not print if disabled", {
        expect_POST(
            expect_output(crPOST("/api/", body='{"value":1}'),
                NA),
            "/api/",
            '{"value":1}')
    })
    test_that("crunch.debug logging if enabled", {
        with(temp.option(crunch.debug=TRUE), {
            expect_POST(
                expect_output(crPOST("/api/", body='{"value":1}'),
                    '\n {"value":1} \n',
                    fixed=TRUE),
                "/api/",
                '{"value":1}')
        })
    })
})

if (run.integration.tests) {
    test_that("Request headers", {
        skip_on_jenkins("Don't fail the build if httpbin is down")
        r <- try(crGET("http://httpbin.org/gzip"))
        expect_true(r$gzipped)
        expect_true(grepl("gzip", r$headers[["Accept-Encoding"]]))
        expect_true(grepl("rcrunch", r$headers[["User-Agent"]]))
    })

    test_that("crunchUserAgent", {
        expect_true(grepl("rcrunch", crunchUserAgent()))
        expect_true(grepl("libcurl", crunchUserAgent()))
        expect_error(crunchUserAgent("anotherpackage/3.1.4"),
            NA)
        expect_true(grepl("anotherpackage", crunchUserAgent("anotherpackage")))
    })

    with_test_authentication({
        test_that("API root can be fetched", {
            expect_true(is.shojiObject(getAPIRoot()))
        })
    })

    test_that("API calls throw an error if user is not authenticated", {
        logout()
        expect_error(getAPIRoot(),
            "You are not authenticated. Please `login\\(\\)` and try again.")
    })
}
