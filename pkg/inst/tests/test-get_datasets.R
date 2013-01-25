context("Retrieving dataset list and single datasets")

test_that("selectDatasetFromList", {
    biglist <- list(ds1=5L, ds98=3L)
    expect_identical(selectDatasetFromList("ds1", biglist), 5L)
    expect_error(selectDatasetFromList("ds9", biglist), "ds9 not found")
    expect_error(selectDatasetFromList("ds9", NULL), "ds9 not found")
})

test_that("getUserDatasetURLs gets what we expect", {
    login(test.user)
    ds.url <- createDataset("getdsurl test")
    expect_true(is.character(getUserDatasetURLs()))
    expect_true(ds.url %in% getUserDatasetURLs())
})

test_that("getUserDatasetURLs requires authentication", {
    logout()
    expect_error(getUserDatasetURLs(), 
        "You must authenticate before making this request")
})

test_that("some setup", {
    dflisttest <- data.frame(v1=rnorm(20), v2=letters[1:20], v3=8:27)
    login(test.user)
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
    expect_true(is.list(loadDataset("dflisttest")))
    expect_error(loadDataset("this is totally not a dataset", 
        "this is totally not a dataset not found"))
})

test_that("listDatasets has datasets after a login (and datasets exist)", {
    login(test.user)
    expect_true(length(listDatasets())>0)
})