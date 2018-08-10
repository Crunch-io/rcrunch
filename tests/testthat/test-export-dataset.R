context("Export dataset")

with_mock_crunch({
    ds <- loadDataset("test ds")
    test_that("Export POST request", {
        expect_POST(
            exportDataset(ds, file = ""),
            "https://app.crunch.io/api/datasets/1/export/csv/",
            '{"filter":null,"options":{"use_category_ids":false}}'
        )
        expect_POST(
            write.csv(ds, file = ""),
            "https://app.crunch.io/api/datasets/1/export/csv/",
            '{"filter":null,"options":{"use_category_ids":false}}'
        )
    })
    test_that("with categorical='id'", {
        expect_POST(
            write.csv(ds, file = "", categorical = "id"),
            "https://app.crunch.io/api/datasets/1/export/csv/",
            '{"filter":null,"options":{"use_category_ids":true}}'
        )
    })
    test_that("with na", {
        expect_POST(
            write.csv(ds, file = "", na = ""),
            "https://app.crunch.io/api/datasets/1/export/csv/",
            '{"filter":null,"options":{"use_category_ids":false,',
            '"missing_values":""}}'
        )
        expect_POST(
            write.csv(ds, file = "", na = "."),
            "https://app.crunch.io/api/datasets/1/export/csv/",
            '{"filter":null,"options":{"use_category_ids":false,',
            '"missing_values":"."}}'
        )
    })
    test_that("Export SPSS request", {
        expect_POST(
            exportDataset(ds, file = "", format = "spss"),
            "https://app.crunch.io/api/datasets/1/export/spss/",
            '{"filter":null,"options":{"var_label_field":"name"}}'
        )
    })
    test_that("Export SPSS ignores 'categorical' arg", {
        expect_POST(
            exportDataset(ds, file = "", format = "spss", categorical = "zzzz"),
            "https://app.crunch.io/api/datasets/1/export/spss/",
            '{"filter":null,"options":{"var_label_field":"name"}}'
        )
    })
    test_that("Export SPSS request with varlabel", {
        expect_POST(
            exportDataset(ds, file = "", format = "spss", varlabel = "description"),
            "https://app.crunch.io/api/datasets/1/export/spss/",
            '{"filter":null,"options":{"var_label_field":"description"}}'
        )
    })
    test_that("Export with additional options", {
        expect_POST(
            exportDataset(ds, file = "", format = "spss", otheropt = TRUE),
            "https://app.crunch.io/api/datasets/1/export/spss/",
            '{"filter":null,"options":{"otheropt":true,"var_label_field":"name"}}'
        )
    })
    test_that("Unsupported arg values", {
        expect_error(
            exportDataset(ds, format = "exe"),
            "'arg' should be one of "
        )
        expect_error(
            exportDataset(ds, format = "spss", varlabel = "NOTAVARLAB"),
            "'arg' should be one of "
        )
        expect_error(
            exportDataset(ds, categorical = "NOT"),
            "'arg' should be one of "
        )
    })

    test_that("Exporting only one variable", {
        expect_POST(
            write.csv(ds["gender"], file = ""),
            "https://app.crunch.io/api/datasets/1/export/csv/",
            '{"filter":null,"where":{"function":"select",',
            '"args":[{"map":{"66ae9881e3524f7db84970d556c34552":',
            '{"variable":"https://app.crunch.io/api/datasets/1/variables/gender/"}}}]},',
            '"options":{"use_category_ids":false}}'
        )
    })

    test_that("Exporting duplicate variable references is prevented", {
        expect_error(
            write.csv(ds[c("gender", "birthyr", "gender")], file = ""),
            "Duplicate variable reference: gender"
        )
        expect_error(
            write.csv(ds[c("gender", "birthyr", "gender", "gender", "birthyr")], file = ""),
            "Duplicate variable references: gender and birthyr"
        )
    })
})

validExport <- function(df2) {
    expect_identical(dim(df2), dim(ds))
    expect_equal(df2$v3, df$v3)
    expect_identical(levels(df2$v4), c("B", "C"))
    ## assert more
}

with_test_authentication({
    ds <- newDataset(df)
    test_that("Can download a csv of a dataset", {
        skip_on_local_backend("Vagrant host doesn't serve files correctly")
        filename <- tempfile()
        exportDataset(ds, file = filename)
        df2 <- read.csv(filename)
        validExport(df2)
    })

    test_that("write.csv alias", {
        skip_on_local_backend("Vagrant host doesn't serve files correctly")
        filename <- tempfile()
        write.csv(ds, file = filename)
        df2 <- read.csv(filename)
        validExport(df2)
    })

    test_that("Can filter rows in export", {
        skip_on_local_backend("Vagrant host doesn't serve files correctly")
        filename <- tempfile()
        write.csv(ds[ds$v4 == "C", ], file = filename)
        df2 <- read.csv(filename)
        expect_identical(nrow(df2), 10L)
        expect_identical(levels(df2$v4), "C")
    })

    test_that("Can filter columns in export", {
        skip_on_local_backend("Vagrant host doesn't serve files correctly")
        filename <- tempfile()
        write.csv(ds[, c("v2", "v4")], file = filename)
        df2 <- read.csv(filename)
        expect_identical(dim(df2), c(20L, 2L))
        expect_identical(names(df2), c("v2", "v4"))
    })

    test_that("Can filter rows and columns in export", {
        skip_on_local_backend("Vagrant host doesn't serve files correctly")
        filename <- tempfile()
        write.csv(ds[ds$v4 == "C", c("v2", "v4")], file = filename)
        df2 <- read.csv(filename)
        expect_identical(dim(df2), c(10L, 2L))
        expect_identical(names(df2), c("v2", "v4"))
        expect_identical(levels(df2$v4), "C")
    })

    test_that("Can export category ids", {
        skip_on_local_backend("Vagrant host doesn't serve files correctly")
        filename <- tempfile()
        write.csv(ds[, c("v2", "v4")], file = filename, categorical = "id")
        df2 <- read.csv(filename)
        expect_identical(dim(df2), c(20L, 2L))
        expect_equal(names(df2), c("v2", "v4"))
        expect_is(df2$v4, "integer")
        expect_equal(df2$v4, as.vector(ds$v4, mode = "id"))
    })

    test_that("Exclusion is applied, even if it depends on column not selected", {
        skip_on_local_backend("Vagrant host doesn't serve files correctly")
        filename <- tempfile()
        exclusion(ds) <- ds$v4 == "C"
        write.csv(ds[, c("v2", "v3")], file = filename)
        df2 <- read.csv(filename)
        expect_identical(dim(df2), c(10L, 2L))
        expect_identical(names(df2), c("v2", "v3"))
    })

    test_that("Exclusion is applied, even if it depends on column not selected", {
        skip_on_local_backend("Vagrant host doesn't serve files correctly")
        filename <- tempfile()
        exclusion(ds) <- NULL
        ds$v4[8] <- NA
        write.csv(ds[, c("v1", "v2", "v4")], file = filename, na = "NANANA")
        csvlines <- readLines(filename)
        expect_equal(grep("NANANA", csvlines), c(2:6, 9, 17:21))
        df2 <- read.csv(filename, na.strings = "NANANA")
        expect_identical(dim(df2), c(20L, 3L))
        expect_identical(names(df2), c("v1", "v2", "v4"))
        expect_identical(is.na(df2$v1), is.na(df$v1))
    })
})
