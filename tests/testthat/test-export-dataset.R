context("Export dataset")

validExport <- function (df2) {
    expect_identical(dim(df2), dim(ds))
    expect_equal(df2$v3, df$v3)
    expect_identical(levels(df2$v4), c("B", "C"))
    ## assert more
}

if (run.integration.tests) {
    with(test.authentication, {
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
                skip("Implement on filtered-export-118062025")
            })

            test_that("Unsupported export format", {
                expect_error(exportDataset(ds, format="ppt"),
                    "'arg' should be one of ")
            })
        })
    })
}
