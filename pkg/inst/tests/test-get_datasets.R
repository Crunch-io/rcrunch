context("Retrieving dataset list and single datasets")

test_that("selectDatasetFromList", {
    biglist <- list(ds1=5L, ds98=3L)
    expect_identical(selectDatasetFromList("ds1", biglist), 5L)
    expect_error(selectDatasetFromList("ds9", biglist), "ds9 not found")
    expect_error(selectDatasetFromList("ds9", NULL), "ds9 not found")
})

if (!run.only.local.tests) {
    test_that("getUserDatasetURLs gets what we expect", {
        login()
            ds.url <- createDataset("getdsurl test")
            expect_true(is.character(getUserDatasetURLs()))
            expect_true(ds.url %in% getUserDatasetURLs())
        logout()
    })

    test_that("getUserDatasetURLs requires authentication", {
        logout()
        expect_error(getUserDatasetURLs(), 
            "You must authenticate before making this request")
    })
    
    suppressMessages(login())
        test_that("some setup", {
            dflisttest <- df
            newDataset(dflisttest)
        })

        test_that("Dataset objects can be retrieved if authenticated", {
            expect_true(is.list(getDatasetObjects(getUserDatasetURLs())))
            expect_true("dflisttest" %in% names(getDatasetObjects(getUserDatasetURLs())))
        })

        test_that("Dataset list can be retrieved if authenticated", {
            expect_true(is.character(listDatasets()))
            expect_true("dflisttest" %in% listDatasets())
        })

        test_that("A dataset object can be retrieved, if it exists", {
            expect_true(is.dataset(loadDataset("dflisttest")))
            expect_error(loadDataset("this is totally not a dataset", 
                "this is totally not a dataset not found"))
        })
    logout()

    test_that("listDatasets has datasets after a login (and datasets exist)", {
        login()
            expect_true(length(listDatasets())>0)
        logout()
    })
}