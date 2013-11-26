context("Retrieving dataset list and single datasets")

if (!run.only.local.tests) {
    test_that("getDatasetCollection requires authentication", {
        logout()
        expect_error(getDatasetCollection(), 
            "You must authenticate before making this request")
    })
    
    with(test.authentication, {
        test_that("getDatasetCollection gets what we expect", {
            col0 <- getDatasetCollection()
            expect_true(is.list(col0))
            ds.url <- createDataset("getdsurl test")
            col1 <- getDatasetCollection()
            expect_true(is.list(col1))
            expect_equal(length(col1), length(col0) + 1)
            expect_equal(col1[[length(col1)]]$datasetName, "getdsurl test")
            expect_equal(col1[[length(col1)]]$datasetUrl, ds.url)

            ## teardown
            DELETE(ds.url)
            updateDatasetList()
        })
                
        with(test.dataset(df, "dflisttest"), {
            test_that("Dataset list can be retrieved if authenticated", {
                expect_true(is.character(listDatasets()))
                expect_true("dflisttest" %in% listDatasets())
            })

            test_that("A dataset object can be retrieved, if it exists", {
                expect_true(is.dataset(loadDataset("dflisttest")))
                expect_error(loadDataset("this is totally not a dataset", 
                    "this is totally not a dataset not found"))
            })
            test_that("listDatasets has datasets after a login (and datasets exist)", {
                expect_true(length(listDatasets())>0)
            })
        })
    })
}