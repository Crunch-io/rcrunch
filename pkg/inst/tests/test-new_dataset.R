context("making a new dataset")

if (!run.only.local.tests) {
    test_that("Source file can be uploaded if logged in", {
        login(test.user)
            testfile <- system.file("fake.csv", package="rcrunch",
                mustWork=TRUE)
            expect_true(createSource(testfile, 
                response.handler=function (response) response$status_code==201))
        logout()
    })

    test_that("Source file cannot be uploaded if not logged in", {
        logout()
        testfile <- system.file("fake.csv", package="rcrunch", mustWork=TRUE)
        expect_error(createSource(testfile), 
            "You must authenticate before making this request")
    })

    test_that("Dataset container object can be created if logged in", {
        login(test.user)
            expect_true(createDataset("testfile", 
                response.handler=function (response) response$status_code==201))
        logout()
    })

    test_that("Dataset container object cannot be created if not logged in", {
        logout()
        expect_error(createDataset("testfile"), 
            "You must authenticate before making this request")
    })

    test_that("Source can be added to Dataset", {
        login(test.user)
            testfile <- system.file("fake.csv", package="rcrunch",
                mustWork=TRUE)
            source <- createSource(testfile)
            ds <- createDataset("add source test")
            expect_true(addSourceToDataset(ds, source, 
                response.handler=function (response) response$status_code==204))
        logout()
    })
    
    test_that("Dataset can be made from a data.frame", {
        login(test.user)
            making_a_dataset_from_df <- df
            newDataset(making_a_dataset_from_df)
            expect_true("making_a_dataset_from_df" %in% listDatasets())
            testcrdf <- newDataset(df)
            expect_true(is.dataset(testcrdf))
        logout()
    })
}

