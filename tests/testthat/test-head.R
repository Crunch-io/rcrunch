context("head and tail")

with_mock_crunch({
    ds <- loadDataset("test ds")
    ds2 <- ds[, "textVar"]
    test_that("head method works on CrunchDatasets", {
        head_df <- head(ds2)
        expect_is(head_df, "data.frame")
        expect_equal(nrow(head_df), 6)
        expect_identical(head_df$textVar, c("w", "n", "x", "b", "q", "s"))
    })
    test_that("head method works on CrunchDataFrame", {
        expect_identical(head(ds2), head(as.data.frame(ds2)))
    })
    test_that("tail generates the correct request", {
        expect_GET(tail(as.data.frame(ds)),
            'https://app.crunch.io/api/datasets/1/variables/birthyr/values',
            '?filter=%7B%22function%22%3A%22between%22%2C%22args%22%3A%5B%7B%22function%22%3A%22row%22%2C%22args%22%3A%5B%5D%7D%2C%7B%22value%22%3A19%7D%2C%7B%22value%22%3A25%7D%5D%7D&offset=0&limit=100000')
    })
    expect_identical(tail(ds$textVar), c("i", "h", "z", "m", "c", "x"))
})

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
