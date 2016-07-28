context("Making a new dataset")

test_that("fake.csv is what we expect", {
    expect_identical(dim(testfile.df), c(20L, 6L))
})

test_that("newDataset input validation", {
    expect_error(newDataset(NULL),
        "Can only make a Crunch dataset from a two-dimensional data")
    expect_error(newDataset(1:5),
        "Can only make a Crunch dataset from a two-dimensional data")
})

if (run.integration.tests) {
    test_that("Source file cannot be uploaded if not logged in", {
        logout()
        expect_error(createSource(testfile.csv),
            "You must authenticate before making this request")
    })
    test_that("Dataset container object cannot be created if not logged in", {
        logout()
        expect_error(createDataset("testfile.csv"),
            "You must authenticate before making this request")
    })

    with_test_authentication({
        test_that("newDatasetFromFile creates a dataset", {
            ds <- newDatasetFromFile(testfile.csv)
            expect_true(is.dataset(ds))
            expect_identical(nrow(ds), 20L)
            expect_identical(ncol(ds), 6L)
            expect_equivalent(mean(ds[[2]]), mean(testfile.df[[2]]))
        })

        test_that("Dataset-by-column variable types get set correctly", {
            ds <- newDatasetByColumn(df)
            expect_valid_df_import(ds)
            expect_equivalent(mean(ds$v3), mean(df$v3))
            expect_true(setequal(names(df), names(ds)))
            expect_identical(names(df), names(ds))
        })

        test_that("newDataset via CSV + JSON", {
            ds <- newDatasetByCSV(df)
            expect_valid_df_import(ds)
        })

        test_that("createWithMetadataAndFile using docs example", {
            ds <- newDatasetFromFixture("apidocs")
            expect_true(is.dataset(ds))
            expect_identical(name(ds), "Example dataset")
            expect_identical(names(categories(ds$q1)),
                c("Cat", "Dog", "Bird", "Skipped", "Not Asked"))
        })

        m <- fromJSON(file.path("dataset-fixtures", "apidocs.json"),
            simplifyVector=FALSE)

        test_that("Can create dataset with data in S3", {
            ds <- createWithMetadataAndFile(m,
                file="s3://public.testing.crunch.io/example-dataset.csv")
            expect_valid_apidocs_import(ds)
            ds2 <- newDatasetFromFixture("apidocs")
            ## Compare to dataset imported from local file upload
            expect_identical(dim(ds), dim(ds2))
            expect_identical(as.vector(ds$q1), as.vector(ds2$q1))
            ## Could add more assertions
        })

        test_that("Duplicate subvariables are forbidden", {
            m2 <- m
            ## Add a duplicate subvariable
            m2$body$table$metadata$allpets$subvariables[[4]] <- list(name="Another", alias="allpets_1")
            expect_error(createWithMetadataAndFile(m2,
                file.path("dataset-fixtures", "apidocs.csv")))
        })

        test_that("newDataset without specifying name grabs object name", {
            dsz <- newDataset(df)
            expect_true(is.dataset(dsz))
            expect_identical(name(dsz), "df")
            expect_valid_df_import(dsz)
        })

        test_that("Datasets can be deleted", {
            dsname <- uniqueDatasetName()
            testdf <- createDataset(name=dsname)
            expect_true(dsname %in% listDatasets())
            expect_true(isTRUE(crDELETE(self(testdf),
                status.handlers=list(`204`=function (response) TRUE))))
            expect_false(dsname %in% listDatasets(refresh=TRUE))
        })
        test_that("Datasets can be deleted by S4 method", {
            dsname <- uniqueDatasetName()
            testdf <- createDataset(name=dsname)
            expect_true(dsname %in% listDatasets())
            delete(testdf)
            expect_false(dsname %in% listDatasets())
        })
    })
}

## TODO:
## 1) Test "cleanup" path, and perhaps broaden to all newDataset methods
## 2) Test "strict" option, and perhaps move it to batch payload instead of query param
