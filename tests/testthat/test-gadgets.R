context("Shiny gadgets")

test_that("callFromOtherPackage", {
    NOTAFUNCTION <- sum <- function (x) {
        call <- match.call()
        callFromOtherPackage(call, "base")
    }
    expect_equal(sum(1:4), 10)
    expect_error(NOTAFUNCTION(1:4),
        "Please install the latest version of base to access this function")
})
