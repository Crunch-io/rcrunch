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
            with(test.dataset(df, "getdsurl test"), {
                testdf <- .setup
                col1 <- getDatasetCollection()
                expect_true(is.list(col1))
                expect_equal(length(col1), length(col0) + 1)
                expect_equal(col1[[length(col1)]]$datasetName, "getdsurl test")
                expect_equal(col1[[length(col1)]]$datasetUrl, self(testdf))
            })
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
                expect_true(is.dataset(loadDataset(1)))
                expect_error(loadDataset(999))
            })
            test_that("loadDataset respects useAlias", {
                expect_equal(loadDataset("dflisttest")@useAlias,
                    default.useAlias())
                expect_false(loadDataset("dflisttest", useAlias=FALSE)@useAlias)
            })
            test_that("listDatasets has datasets after a login (and datasets exist)", {
                expect_true(length(listDatasets())>0)
            })
        })
    })
}