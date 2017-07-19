context("Retrieving dataset list and single datasets")

with_mock_crunch({
    cr <- session()
    test_that("listDatasets lists", {
        expect_identical(listDatasets(), c("ECON.sav", "test ds"))
        expect_identical(listDatasets("archived"), "an archived dataset")
        expect_identical(listDatasets("all"), c("ECON.sav", "an archived dataset", "test ds"))
    })

    test_that("loadDataset loads", {
        ## NOTE: implementing the active dataset loading only first
        ds <- loadDataset("test ds")
        expect_true(is.dataset(ds))
        expect_identical(name(ds), "test ds")
        ds <- loadDataset(2)
        expect_true(is.dataset(ds))
        expect_identical(name(ds), "test ds")
        expect_error(loadDataset(666), "subscript out of bounds")
        expect_error(loadDataset("not a dataset"),
            paste(dQuote("not a dataset"), "not found"))
    })

    test_that("loadDataset loads with DatasetTuple", {
        ds <- loadDataset(cr$datasets[["test ds"]])
        expect_true(is.dataset(ds))
        expect_identical(name(ds), "test ds")
        expect_true(is.dataset(loadDataset(cr$datasets$`test ds`)))
    })

    test_that("loadDataset by URL when in main catalog", {
        ds <- loadDataset("test ds")
        ## This URL is found in dataset <- dscat[[dataset]] because of getNameOrURL
        expect_identical(ds, loadDataset(self(ds)))
    })
    test_that("loadDataset by URL when not in main catalog", {
        ds2 <- loadDataset("https://app.crunch.io/api/datasets/four/")
        expect_is(ds2, "CrunchDataset")
        expect_identical(self(ds2),
            "https://app.crunch.io/api/datasets/four/")
        expect_identical(name(ds2), "mtcars from R")
    })

    test_that("loadDataset by web app URL", {
        ds3 <- loadDataset("https://app.crunch.io/dataset/3/browse")
        expect_is(ds3, "CrunchDataset")
        expect_identical(name(ds3), "ECON.sav")
    })

    ds <- loadDataset("test ds")
    with_consent({
        test_that("deleteDataset by name", {
            expect_DELETE(deleteDataset("test ds"), self(ds))
        })
        test_that("deleteDataset by index", {
            expect_DELETE(deleteDataset(2), self(ds))
        })
        test_that("deleteDataset on Dataset object", {
            expect_DELETE(deleteDataset(ds), self(ds))
        })
    })

    test_that("deleteDataset error handling", {
        expect_error(deleteDataset("this is totally not a dataset",
            paste(dQuote("this is totally not a dataset"), "not found")))
        expect_error(deleteDataset(9999),
            "subscript out of bounds")
        expect_error(deleteDataset(ds), "Must confirm")
    })
})

with_test_authentication({
    test_that("datasets() gets the dataset catalog", {
        col0 <- datasets()
        expect_is(col0, "DatasetCatalog")
        with(test.dataset(), {
            col1 <- datasets()
            expect_is(col1, "DatasetCatalog")
            expect_equal(length(col1), length(col0) + 1)
        })
    })
    with(test.dataset(), {
        dsname <- name(ds)
        test_that("Dataset list can be retrieved if authenticated", {
            expect_true(is.character(listDatasets()))
            expect_true(length(listDatasets())>0)
            expect_true(is.character(dsname))
            expect_true(nchar(dsname) > 0)
            expect_true(dsname %in% listDatasets())
        })

        test_that("A dataset object can be retrieved, if it exists", {
            expect_true(is.dataset(loadDataset(dsname)))
            dsnum <- which(listDatasets() %in% dsname)
            expect_true(is.numeric(dsnum))
            expect_true(is.dataset(loadDataset(dsnum)))
        })
    })

    with(test.dataset(), {
        dsname <- name(ds)
        newname <- paste0("New name ", now())

        test_that("renaming a dataset refreshes the dataset list", {
            expect_true(dsname %in% listDatasets())
            name(ds) <- newname
            expect_false(dsname %in% listDatasets())
            expect_true(newname %in% listDatasets())
        })

        test_that("deleting a dataset refreshes the dataset list", {
            with_consent(delete(ds))
            expect_false(dsname %in% listDatasets())
            expect_false(newname %in% listDatasets())
        })
    })
})
