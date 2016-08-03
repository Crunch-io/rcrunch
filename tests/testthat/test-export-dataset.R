context("Export dataset")

with_mock_HTTP({
    ds <- loadDataset("test ds")
    test_that("Export POST request", {
        expect_POST(exportDataset(ds, file=""),
            '/api/datasets/dataset1/export/csv/',
            '{"filter":null,"options":{"use_category_ids":false}}')
        expect_POST(write.csv(ds, file=""),
            '/api/datasets/dataset1/export/csv/',
            '{"filter":null,"options":{"use_category_ids":false}}')
    })
    test_that("with categorical='id'", {
        expect_POST(write.csv(ds, file="", categorical="id"),
            '/api/datasets/dataset1/export/csv/',
            '{"filter":null,"options":{"use_category_ids":true}}')
    })
    test_that("Export SPSS request", {
        expect_POST(exportDataset(ds, file="", format="spss"),
            '/api/datasets/dataset1/export/spss/',
            '{"filter":null}')
    })
    test_that("Export SPSS ignores 'categorical' arg", {
        expect_POST(exportDataset(ds, file="", format="spss", categorical="zzzz"),
            '/api/datasets/dataset1/export/spss/',
            '{"filter":null}')
    })
    test_that("Unsupported export format", {
        expect_error(exportDataset(ds, format="exe"),
            "'arg' should be one of ")
    })
})

validExport <- function (df2) {
    expect_identical(dim(df2), dim(ds))
    expect_equal(df2$v3, df$v3)
    expect_identical(levels(df2$v4), c("B", "C"))
    ## assert more
}

if (run.integration.tests) {
    with_test_authentication({
        with(test.dataset(df), {
            test_that("Can download a csv of a dataset", {
                filename <- tempfile()
                exportDataset(ds, file=filename)
                df2 <- read.csv(filename)
                validExport(df2)
            })

            test_that("write.csv alias", {
                filename <- tempfile()
                write.csv(ds, file=filename)
                df2 <- read.csv(filename)
                validExport(df2)
            })

            test_that("Can filter rows in export", {
                filename <- tempfile()
                write.csv(ds[ds$v4 == "C",], file=filename)
                df2 <- read.csv(filename)
                expect_identical(nrow(df2), 10L)
                expect_identical(levels(df2$v4), "C")
            })

            test_that("Can filter columns in export", {
                filename <- tempfile()
                write.csv(ds[,c("v2", "v4")], file=filename)
                df2 <- read.csv(filename)
                expect_identical(dim(df2), c(20L, 2L))
                expect_identical(names(df2), c("v2", "v4"))
            })

            test_that("Can filter rows and columns in export", {
                filename <- tempfile()
                write.csv(ds[ds$v4 == "C", c("v2", "v4")], file=filename)
                df2 <- read.csv(filename)
                expect_identical(dim(df2), c(10L, 2L))
                expect_identical(names(df2), c("v2", "v4"))
                expect_identical(levels(df2$v4), "C")
            })

            test_that("Can export category ids", {
                filename <- tempfile()
                write.csv(ds[, c("v2", "v4")], file=filename, categorical="id")
                df2 <- read.csv(filename)
                expect_identical(dim(df2), c(20L, 2L))
                expect_equal(names(df2), c("v2", "v4"))
                expect_is(df2$v4, "integer")
                expect_equal(df2$v4, as.vector(ds$v4, mode="id"))
            })
        })
    })
}
