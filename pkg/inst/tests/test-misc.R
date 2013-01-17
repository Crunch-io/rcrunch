context("Various helper functions")

test_that("is.error", {
    expect_true(is.error(try(stop(""), silent=TRUE)))
    expect_false(is.error("not an error"))
    expect_false(is.error(NULL))
    expect_false(is.error(NA))
})

test_that("update list", {
    a <- list(a=1, b=2)
    b <- list(c=3, b=4)
    expect_identical(update.list(a, b), list(a=1, b=4, c=3))
    expect_identical(update.list(list(), b), b)
    expect_identical(update.list(NULL, b), b)
})