context("Retrieving dataset list and single datasets")

with_mock_HTTP({
    dataset.catalog.url <- "/api/datasets.json"
    datcat <- DatasetCatalog(crGET(dataset.catalog.url))
    index(datcat)[[which(names(datcat) == "an archived dataset")]]$archived <- TRUE
    session_store$datasets <- datcat ## as if we had done updateDatasetList

    test_that("listDatasets lists", {
        expect_identical(listDatasets(), c("ECON.sav", "test ds"))
        expect_identical(listDatasets("archived"), "an archived dataset")
        expect_identical(listDatasets("all"), c("ECON.sav", "an archived dataset", "test ds"))
    })

    test_that("loadDataset loads", {
        ## NOTE: implementing the active dataset loading only first
        ds <- try(loadDataset("test ds"))
        expect_true(is.dataset(ds))
        expect_identical(name(ds), "test ds")
        ds <- try(loadDataset(2))
        expect_true(is.dataset(ds))
        expect_identical(name(ds), "test ds")
        expect_error(loadDataset(666), "subscript out of bounds")
    })
})

if (run.integration.tests) {
    test_that("updateDatasetList requires authentication", {
        logout()
        expect_error(updateDatasetList(),
            "You must authenticate before making this request")
    })

    with(test.authentication, {
        test_that("datasetCatalog gets what we expect", {
            col0 <- datasetCatalog()
            expect_true(inherits(col0, "DatasetCatalog"))
            with(test.dataset(df), {
                col1 <- datasetCatalog()
                expect_true(inherits(col1, "DatasetCatalog"))
                expect_equal(length(col1), length(col0) + 1)
            })
        })
        with(test.dataset(df), {
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
                expect_error(loadDataset("this is totally not a dataset"),
                    paste(dQuote("this is totally not a dataset"), "not found"))
                dsnum <- which(listDatasets() %in% dsname)
                expect_true(is.numeric(dsnum))
                expect_true(is.dataset(loadDataset(dsnum)))
                expect_error(loadDataset(9999))
            })
            test_that("loadDataset respects useAlias", {
                expect_equal(loadDataset(dsname)@useAlias,
                    default.useAlias())
                expect_false(loadDataset(dsname, useAlias=FALSE)@useAlias)
            })

            test_that("deleteDataset by name", {
                expect_error(deleteDataset("this is totally not a dataset",
                    paste(dQuote("this is totally not a dataset"), "not found")))
                out <- try(deleteDataset(dsname))
                expect_false(dsname %in% listDatasets())
            })
        })

        with(test.dataset(df), {
            dsname <- name(ds)
            dsnum <- which(listDatasets() %in% dsname)
            test_that("deleteDataset by index", {
                expect_true(dsname %in% listDatasets())

                expect_error(deleteDataset(dsnum + 9999),
                    "subscript out of bounds")
                out <- deleteDataset(dsnum)
                expect_false(dsname %in% listDatasets())
            })
        })

        with(test.dataset(df), {
            dsname <- name(ds)
            test_that("deleteDataset on Dataset object", {
                expect_true(dsname %in% listDatasets())
                out <- deleteDataset(ds)
                expect_false(dsname %in% listDatasets())
            })
        })

        with(test.dataset(df), {
            dsname <- name(ds)
            newname <- paste0("New name ", now())

            test_that("renaming a dataset refreshes the dataset list", {
                expect_true(dsname %in% listDatasets())
                name(ds) <- newname
                expect_false(dsname %in% listDatasets())
                expect_true(newname %in% listDatasets())
            })

            test_that("deleting a dataset refreshes the dataset list", {
                delete(ds)
                expect_false(dsname %in% listDatasets())
                expect_false(newname %in% listDatasets())
            })
        })
    })
}
