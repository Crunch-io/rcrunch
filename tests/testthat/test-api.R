context("API calling")

test_that("Deleted endpoints tell user to upgrade", {
    fake410 <- fake_response("http://crunch.io/410", status_code = 410)
    expect_error(
        handleAPIresponse(fake410),
        paste(
            "The API resource at http://crunch.io/410 has moved permanently.",
            "Please upgrade crunch to the latest version."
        )
    )
})


test_that("401 errors give informative errors", {
    fake401 <- fake_response("http://crunch.io/401", status_code = 401)
    with(temp.option(crunch = list(crunch.api = "url", crunch.api.key = "key")), {
        expect_error(
            handleAPIresponse(fake401),
            "Could not connect to 'url' with key set using"
        )
    })

    with(temp.option(crunch = list(crunch.api.key = "")), {
        expect_error(
            handleAPIresponse(fake401),
            "No authentication key found. See"
        )
    })
})

test_that("get_header", {
    expect_identical(get_header("bar", list(bar = 5)), 5)
    expect_identical(get_header("foo", list(bar = 5)), NULL)
    expect_identical(get_header("foo", list(bar = 5), default = 42), 42)
})

test_that("get_crunch_auth_config works", {
    with(temp.option(
        crunch = list(crunch.api = "https://app.crunch.io/api/", crunch.api.key = "key")
    ), {
        # sends to same host
        expect_equal(
            get_crunch_auth_config("https://app.crunch.io/api/datasets/"),
            add_headers(Authorization = paste0("Bearer ", "key"))
        )
        # Also sends to other crunch.io subdomains
        expect_equal(
            get_crunch_auth_config("https://testing.crunch.io/api/datasets/"),
            add_headers(Authorization = paste0("Bearer ", "key"))
        )
        # But are not send outside of crunch.io
        expect_equal(
            get_crunch_auth_config("https://example.com"),
            httr::add_headers()
        )
    })
})

with_mock_crunch({
    test_that("Deprecation warnings report to the user", {
        expect_warning(
            resp <- crGET("https://app.crunch.io/deprecated/"),
            paste(
                "The API resource at https://app.crunch.io/api/ returned a",
                "deprecation warning. Updating to the latest version of the",
                "package is recommended and may resolve the issue. Details:",
                '299 - "This resource is scheduled for removal on 2018-03-20"'
            )
        )
    })

    test_that("But other kinds of warnings don't look like deprecations", {
        expect_warning(
            resp <- crGET("https://app.crunch.io/other-warning/"),
            paste(
                "The API resource at https://app.crunch.io/api/ returned a",
                'warning. Details: 298 - "This is some other kind of warning"'
            )
        )
    })

    test_that("crunch.debug does not print if disabled", {
        expect_POST(
            expect_prints(
                crPOST("https://app.crunch.io/api/", body = '{"value":1}'),
                NA
            ),
            "https://app.crunch.io/api/",
            '{"value":1}'
        )
    })
    test_that("crunch.debug logging if enabled", {
        with(temp.option(crunch = list(crunch.debug = TRUE)), {
            expect_POST(
                expect_prints(crPOST("https://app.crunch.io/api/", body = '{"value":1}'),
                    '\n {"value":1} \n',
                    fixed = TRUE
                ),
                "https://app.crunch.io/api/",
                '{"value":1}'
            )
            ## Use testthat:: so that it doesn't print ds. Check for log printing
            expect_output(
                ds <- cachedLoadDataset("test ds"),
                NA
            )
        })
    })
    test_that("503 on GET with Retry-After is handled", {
        expect_message(
            resp <- crGET("https://app.crunch.io/503/"),
            "This request is taking longer than expected. Please stand by..."
        )
        expect_identical(resp, crGET("https://app.crunch.io/api/"))
    })
    test_that("404 error message includes the request URL", {
        expect_error(
            resp <- crGET("https://app.crunch.io/404/"),
            "Client error: (404) Not Found: https://app.crunch.io/404/",
            fixed = TRUE
        )
    })

    test_that("Checking feature flags", {
        expect_true(featureFlag("this_is_on"))
        expect_false(featureFlag("this_is_off"))
    })

    test_that("crDELETE passes on and calls drop= args", {
        msg <- "Drop and roll"
        expect_DELETE(
            crDELETE("https://app.crunch.io/", drop = stop(msg)),
            "https://app.crunch.io/"
        )
        expect_error(
            crDELETE("https://app.crunch.io/delete-me", drop = stop(msg)),
            "Drop and roll"
        )
    })
})

test_that("retry", {
    counter <- 0
    f <- function() {
        counter <<- counter + 1
        stopifnot(counter == 3)
        return(counter)
    }
    expect_identical(retry(f(), wait = .001), 3)
    counter <- 0
    expect_error(
        retry(f(), wait = .001, max.tries = 2),
        "counter == 3 is not TRUE"
    )
})

if (run.integration.tests) {
    test_that("Request headers", {
        skip_if_disconnected()
        r <- crGET("http://httpbin.org/gzip")
        expect_true(r$gzipped)
        expect_true(grepl("gzip", r$headers[["Accept-Encoding"]]))
        expect_true(grepl("rcrunch", r$headers[["User-Agent"]]))
    })

    test_that("crunch_user_agent", {
        expect_true(grepl("rcrunch", crunch_user_agent()))
        expect_true(grepl("libcurl", crunch_user_agent()))
        expect_error(
            crunch_user_agent("anotherpackage/3.1.4"),
            NA
        )
        expect_true(grepl("anotherpackage", crunch_user_agent("anotherpackage")))
    })

    with_test_authentication({
        test_that("API root can be fetched", {
            expect_true(is.shojiObject(getAPIRoot()))
        })
    })

    test_that("API calls throw an error if user is not authenticated", {
        with(temp.option(crunch = list(crunch.api.key = "")), {
            expect_error(
                getAPIRoot(),
                "No authentication key found. See `help('crunch-api-key')` for more information.",
                fixed = TRUE
            )
        })

        with(temp.option(crunch = list(crunch.api.key = "xyz")), {
            expect_error(getAPIRoot(), "Could not connect to.+")
        })
    })
}
