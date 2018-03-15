context("head and tail")

with_mock_crunch({
    with_silent_progress({
        ds <- loadDataset("test ds")
        with_mock(`crunch:::crDownload`=function (url, file) {
            resp <- GET(url)
            return(resp$content)
        }, {
            test_that("head method works on CrunchDatasets", {
                head_df <- head(ds[,c("birthyr", "gender", "location", "mymrset", "textVar", "starttime")])
                expect_is(head_df, "data.frame")
                expect_equal(nrow(head_df), 6)
                expect_identical(head_df$textVar, c("w", "n", "x", "b", "q", "s"))
            })
            test_that("tail generates the correct request", {
                expect_POST(tail(as.data.frame(ds)),
                    'https://app.crunch.io/api/datasets/1/export/csv/',
                    '{"filter":{"function":"between","args":[{"function":"row","args":[]},{"value":19},{"value":25}]},"options":{"use_category_ids":true}')
            })
            test_that("head and tail work on CrunchVariables", {
                expect_identical(head(ds$textVar),  c("w", "n", "x", "b", "q", "s"))
                expect_identical(tail(ds$textVar), c("i", "h", "z", "m", "c", "x"))
            })
        })
    })
})

with_test_authentication({
    ds <- newDatasetFromFixture("apidocs")
    ds_df <- as.data.frame(ds, force = TRUE)
    dfhead <- head(ds)

    test_that("head on dataset", {
        expect_identical(dim(dfhead), c(6L, ncol(ds_df)))
        expect_identical(dfhead$q1, ds_df$q1[1:6])

        expect_identical(dim(tail(ds)), c(6L, ncol(ds_df)))
        expect_identical(tail(ds)$ndogs, tail(ds_df$ndogs))

    })
    test_that("head and tail on CrunchDataFrame", {
        expect_identical(dim(head(as.data.frame(ds))), c(6L, ncol(ds_df)))
        expect_identical(head(as.data.frame(ds)$q1), head(ds_df$q1))

        expect_identical(dim(tail(as.data.frame(ds))), c(6L, ncol(ds_df)))
        expect_identical(tail(as.data.frame(ds)$q1), tail(ds_df$q1))
    })

    test_that("head on Variable", {
        expect_identical(head(ds$q1), head(ds_df$q1))
        expect_identical(tail(ds$q1), tail(ds_df$q1))
    })
    test_that("head and tail on subsets of Dataset", {
        ds_filt <- ds[ds$ndogs > 1, ]
        df_filt <- as.data.frame(ds_filt, force = TRUE)
        expect_identical(dim(head(ds_filt)), c(6L, ncol(ds_df)))
        expect_identical(head(ds_filt)$q1, df_filt$q1[1:6])

        expect_identical(dim(tail(ds_filt)), c(6L, ncol(ds_df)))
        expect_identical(tail(ds_filt)$ndogs, tail(df_filt$ndogs))

        #Filtered variables
        expect_identical(head(ds_filt$ndogs), head(df_filt$ndogs))
        expect_identical(tail(ds_filt$ndogs), tail(df_filt$ndogs))
    })
})
