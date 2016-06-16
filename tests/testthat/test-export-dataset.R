context("Export dataset")

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

            test_that("Unsupported export format", {
                expect_error(exportDataset(ds, format="exe"),
                    "'arg' should be one of ")
            })
        })
    })
}
