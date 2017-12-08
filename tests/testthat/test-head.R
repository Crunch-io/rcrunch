context("head and tail")

with_test_authentication({
    ds <- newDatasetFromFixture("apidocs")
    df <- as.data.frame(ds, force = TRUE)
    dfhead <- head(ds)

    test_that("head on dataset", {
        expect_identical(dim(dfhead), c(6L, ncol(df)))
        expect_identical(dfhead$q1, df$q1[1:6])

        expect_identical(dim(tail(ds)), c(6L, ncol(df)))
        expect_identical(tail(ds)$ndogs, tail(df$ndogs))

    })
    test_that("head and tail on CrunchDataFrame", {
        expect_identical(dim(head(as.data.frame(ds))), c(6L, ncol(df)))
        expect_identical(head(as.data.frame(ds)$ndogs), head(df$ndogs))

        expect_identical(dim(tail(as.data.frame(ds))), c(6L, ncol(df)))
        expect_identical(tail(as.data.frame(ds)$ndogs), tail(df$ndogs))
    })

    test_that("head on Variable", {
        expect_identical(head(ds$ndogs), head(df$ndogs))
        expect_identical(tail(ds$ndogs), tail(df$ndogs))
    })
    test_that("head and tail on subsets of Dataset", {
        ds_filt <- ds[ds$ndogs > 1, ]
        df_filt <- as.data.frame(ds_filt, force = TRUE)
        expect_identical(dim(head(ds_filt)), c(6L, ncol(df)))
        expect_identical(head(ds_filt)$q1, df_filt$q1[1:6])

        expect_identical(dim(tail(ds_filt)), c(6L, ncol(df)))
        expect_identical(tail(ds_filt)$ndogs, tail(df_filt$ndogs))

        #Filtered variables
        expect_identical(head(ds_filt$ndogs), head(df_filt$ndogs))
        expect_identical(tail(ds_filt$ndogs), tail(df_filt$ndogs))
    })
})
