context("Appending subsets")

with_test_authentication({
    ds1 <- newDataset(df[1:3])
    ds2 <- newDataset(df[2:5])
    ds1 <- appendDataset(ds1, ds2[c("v2", "v5")])
    test_that("We can select variables to append", {
        expect_equal(ncol(ds1), 4)
        expect_identical(names(ds1), c("v1", "v2", "v3", "v5"))
        asdf <- as.data.frame(ds1)
        expect_equivalent(asdf$v1, c(df$v1, rep(NA_real_, 20)))
        expect_equivalent(asdf$v2, c(df$v2, df$v2))
        expect_equivalent(asdf$v3, c(df$v3, rep(NA_real_, 20)))
        expect_equivalent(asdf$v5, c(rep(as.Date(NA), 20), df$v5))
    })
})
