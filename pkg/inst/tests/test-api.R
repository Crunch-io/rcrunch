context("API calling")

test_that("HTTP verbs are validated", {
    expect_identical("POST", validateVerb("POST"))
    expect_error(validateVerb())
    expect_error(validateVerb(2))
    expect_error(validateVerb(list(foo=4)))
    expect_error(validateVerb("BREW"))
    expect_error(validateVerb(c("PUT", "POST")))
})