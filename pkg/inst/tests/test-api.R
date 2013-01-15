context("API calling")

test_that("HTTP verbs are validated", {
    expect_identical("POST", validateHttpVerb("POST"))
    expect_error(validateHttpVerb())
    expect_error(validateHttpVerb(2))
    expect_error(validateHttpVerb(list(foo=4)))
    expect_error(validateHttpVerb("BREW"))
    expect_error(validateHttpVerb(c("PUT", "POST")))
})

test_that("API root can be fetched", {
    login("***REMOVED***")
    expect_false(is.error(getAPIroot()))
    urls <- getAPIroot()
    expect_true(is.list(urls))
})