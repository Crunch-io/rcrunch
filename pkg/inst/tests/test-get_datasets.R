context("Retrieving dataset list and single datasets")

with(fake.HTTP, {
    dataset.catalog.url <- "api/datasets.json"
    datcat <- do.call("DatasetCatalog", GET(dataset.catalog.url))
    datcat@index[[1]]$archived <- TRUE
    session_store$datasets <- datcat ## as if we had done updateDatasetList
    
    test_that("listDatasets lists", {
        expect_identical(listDatasets(), c("ECON.sav", "test ds"))
        expect_identical(listDatasets("archived"), "an archived dataset")
        expect_identical(listDatasets("all"), c("an archived dataset", "ECON.sav", "test ds"))
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

if (!run.only.local.tests) {
    test_that("updateDatasetList requires authentication", {
        logout()
        expect_error(updateDatasetList(), 
            "You must authenticate before making this request")
    })
    
    with(test.authentication, {
        test_that("datasetCatalog gets what we expect", {
            col0 <- datasetCatalog()
            expect_true(inherits(col0, "DatasetCatalog"))
            with(test.dataset(df, "getdsurl test"), {
                testdf <- .setup
                col1 <- datasetCatalog()
                expect_true(inherits(col1, "DatasetCatalog"))
                expect_equal(length(col1), length(col0) + 1)
            })
        })
        print("yo") 
        with(test.dataset(df, "dflisttest"), {
            test_that("Dataset list can be retrieved if authenticated", {
                expect_true(is.character(listDatasets()))
                expect_true(length(listDatasets())>0)
                expect_true("dflisttest" %in% listDatasets())
            })

            test_that("A dataset object can be retrieved, if it exists", {
                expect_true(is.dataset(loadDataset("dflisttest")))
                print("hello")
                expect_error(loadDataset("this is totally not a dataset", 
                    "this is totally not a dataset not found"))
                expect_true(is.dataset(loadDataset(1)))
                expect_error(loadDataset(999))
            })
            test_that("loadDataset respects useAlias", {
                expect_equal(loadDataset("dflisttest")@useAlias,
                    default.useAlias())
                expect_false(loadDataset("dflisttest", useAlias=FALSE)@useAlias)
            })
        })
    })
}