context("Making a new dataset")

if (!run.only.local.tests) {
    test_that("Source file cannot be uploaded if not logged in", {
        logout()
        testfile <- system.file("fake.csv", package="rcrunch", mustWork=TRUE)
        expect_error(createSource(testfile), 
            "You must authenticate before making this request")
    })
    test_that("Dataset container object cannot be created if not logged in", {
        logout()
        expect_error(createDataset("testfile"), 
            "You must authenticate before making this request")
    })
    
    with(test.authentication, {
        test_that("Source file can be uploaded if logged in", {
            testfile <- system.file("fake.csv", package="rcrunch",
                mustWork=TRUE)
            expect_true(createSource(testfile, 
                response.handler=function (response) response$status_code==201))
        })
        test_that("Dataset container object can be created if logged in", {
            expect_true(createDataset("testfile", 
                response.handler=function (response) response$status_code==201))
        })
        test_that("Source can be added to Dataset", {
            testfile <- system.file("fake.csv", package="rcrunch",
                mustWork=TRUE)
            source <- createSource(testfile)
            ds <- createDataset("add source test")
            expect_equal(addSourceToDataset(ds, source, 
                response.handler=function (response) as.character(response$status_code)), "204")
        })
        test_that("Dataset can be made from a data.frame", {
            expect_error(newDataset(NULL), 
                "Can only make a Crunch dataset from a two-dimensional data")
            expect_error(newDataset(1:5), 
                "Can only make a Crunch dataset from a two-dimensional data")
            making_a_dataset_from_df <- df
            newDataset(making_a_dataset_from_df)
            expect_true("making_a_dataset_from_df" %in% listDatasets())
            testcrdf <- newDataset(df)
            expect_true(is.dataset(testcrdf))
            ## Should also test doing this with a matrix
        })
        test_that("Dataset variable types get set correctly", {
            testdf <- loadDataset("making_a_dataset_from_df")
            expect_true(is.Numeric(testdf[["v1"]]))
            expect_true(is.Text(testdf[["v2"]]))
            expect_true(is.Numeric(testdf[["v3"]]))
            expect_true(is.Categorical(testdf[["v4"]]))
        })
    })
}

