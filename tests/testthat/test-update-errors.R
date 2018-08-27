context("Update error handling")

with_test_authentication({
    ds <- newDataset(df[, 4, drop = FALSE])
    len <- length(as.vector(ds$v4[ds$v4 == "B"]))
    test_that("setup for update with wrong number of values", {
        expect_identical(len, 10L)
    })
    test_that("Trying to update with too many values fails", {
        skip_on_jenkins("Silence the error emails")
        expect_error(
            ds$v4[ds$v4 == "B"] <- rep(1, len + 5),
            "expected 10 values, got 15"
        )
    })
    test_that("Trying to update with too few values fails", {
        skip_on_jenkins("Silence the error emails")
        expect_error(
            ds$v4[ds$v4 == "B"] <- rep(1, len - 3),
            "expected 10 values, got 7"
        )
    })
})
