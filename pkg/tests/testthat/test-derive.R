context("Derive a new variable")

if (run.integration.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            test_that("Can derive a numeric from a numeric", {
                try(ds$v3a <- ds$v3 + 5)
                expect_true(is.Numeric(ds$v3a))
                expect_identical(as.vector(ds$v3a), as.vector(ds$v3) + 5)
                ## Now update v3's values and confirm that v3a is still linked
                try(ds$v3 <- df$v3 + 7)
                expect_equivalent(as.vector(ds$v3), df$v3 + 7)
                expect_identical(as.vector(ds$v3a), as.vector(ds$v3) + 5)
                expect_equivalent(as.vector(ds$v3a), df$v3 + 12)
            })
            test_that("What happens when you try to derive a logical", {
                expect_error(ds$v3b <- ds$v3 < 15,
                    "Cannot currently derive a logical variable")
            })
        })
    })
}