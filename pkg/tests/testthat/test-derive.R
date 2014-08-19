context("Derive a new variable")

if (run.integration.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            try(ds$v3a <- ds$v3 + 5)
            expect_true(is.Numeric(ds$v3a))
            expect_identical(as.vector(ds$v3a), as.vector(ds$v3) + 5)
            ## Now update v3's values and confirm that v3a is still linked
            try(ds$v3 <- df$v3 + 7)
            expect_equivalent(as.vector(ds$v3), df$v3 + 7)
            expect_identical(as.vector(ds$v3a), as.vector(ds$v3) + 5)
            expect_equivalent(as.vector(ds$v3a), df$v3 + 12)
        })
    })
}