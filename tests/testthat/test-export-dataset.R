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

    url <- 'https://app.crunch.io/api/datasets/3/export/csv/'
    post_request <- paste0(
        '{"filter":null,',
        '"where":{"function":"select",',
            '"args":[{"map":{',
                '"66ae9881e3524f7db84970d556c34552":{"variable":"https://app.crunch.io/api/datasets/3/variables/gender/"},',
                '"f78ca47313144b57adfb495893968e70":{"variable":"https://app.crunch.io/api/datasets/3/variables/birthyr/"},',
                '"d7c21314ca9e453c93069168681a285c":{"variable":"https://app.crunch.io/api/datasets/3/variables/starttime/"}}}]'
)
    test_that("exporting hidden variables", {
        ds <- loadDataset("ECON.sav")

        expect_POST(
            write.csv(ds, file = "", include.hidden = TRUE),
            url,
            post_request)

        # ensure that include.hidden is passed down from as.data.frame to write.csv
        expect_POST(
            as.data.frame(ds, include.hidden = TRUE, force = TRUE),
            url,
            post_request)

        subset_post <- paste0(
            '{"filter":null,',
            '"where":{"function":"select",',
            '"args":[{"map":{',
                '"66ae9881e3524f7db84970d556c34552":{"variable":"https://app.crunch.io/api/datasets/3/variables/gender/"},',
                '"f78ca47313144b57adfb495893968e70":{"variable":"https://app.crunch.io/api/datasets/3/variables/birthyr/"}}}]}'
        )
        # Hidden variables can be exported by name without include.hidden
            expect_POST(
                write.csv(ds[, c("gender", "birthyr")], file = ""),
                url,
                subset_post)
            expect_POST(
                as.data.frame(ds[, c("gender", "birthyr")], force = TRUE),
                url,
                subset_post)
    })
    test_that("users can specify all variables by name and get the hidden variables", {
        skip("TODO modify variablesFilter to distinguish between a fully specified variable subset and the original dataset")
        # Currently the API makes us send hidden variables as a where clause to
        # the export API. We work around this by asking for hidden variables when
        # the user specifies them in the variable subset. We don't currently have a
        # good way to do this when the user subsets the dataset with all the variable
        # names. This problem could also go away if the export endpoint allowed us
        # to pass hidden variable names like regular variable names.
        expect_POST(
            write.csv(ds[, c("gender", "birthyr", "starttime")], file = ""),
            url,
            post_request)
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

    test_that("We can export hidden variables", {
        skip_on_local_backend("Vagrant host doesn't serve files correctly")
        ds$hidden_var <- 1:20
        ds <- hideVariables(ds, "hidden_var")
        filename <- tempfile()

        write.csv(ds, file = filename, include.hidden = TRUE)
        df <- read.csv(filename)
        expect_identical(
            names(df),
            c("v1", "v2", "v3", "v4", "v5", "v6", "hidden_var"))

        write.csv(ds, file = filename)
        df <- read.csv(filename)
        expect_identical(
            names(df),
            c("v1", "v2", "v3", "v4", "v5", "v6"))

        # When variable is specified but include.hidden is ommitted
        write.csv(ds[, "hidden_var"], file = filename)
        df <- read.csv(filename)
        expect_identical("hidden_var", names(df))
    })

})
