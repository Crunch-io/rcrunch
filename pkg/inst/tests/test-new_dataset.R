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
            ## Tear down
            updateDatasetList()
            delete(loadDataset("testfile"))
        })
        test_that("Source can be added to Dataset", {
            testfile <- system.file("fake.csv", package="rcrunch",
                mustWork=TRUE)
            source <- createSource(testfile)
            ds <- createDataset("add source test")
            expect_equal(addSourceToDataset(ds, source, 
                response.handler=function (response) as.character(response$status_code)), "201")
            ## Tear down
            updateDatasetList()
            delete(loadDataset("add source test"))
        })
        test_that("Dataset can be made from a data.frame", {
            expect_error(newDataset(NULL), 
                "Can only make a Crunch dataset from a two-dimensional data")
            expect_error(newDataset(1:5), 
                "Can only make a Crunch dataset from a two-dimensional data")
            d1 <- newDataset(df, name="making_a_dataset_from_df")
            expect_true("making_a_dataset_from_df" %in% listDatasets())
            delete(d1)
            testcrdf <- newDataset(df)
            expect_true(is.dataset(testcrdf))
            delete(testcrdf)
            ## Should also test doing this with a matrix
        })
        with(test.dataset(df), {
            testdf <- .setup
            test_that("Dataset variable types get set correctly", {
                expect_true(is.Numeric(testdf[["v1"]]))
                expect_true(is.Text(testdf[["v2"]]))
                expect_true(is.Numeric(testdf[["v3"]]))
                expect_true(is.Categorical(testdf[["v4"]]))
                expect_true(all(levels(df$v4) %in% names(categories(testdf$v4))))
                expect_identical(categories(testdf$v4), categories(refresh(testdf$v4)))
                expect_identical(testdf$v4, refresh(testdf$v4))
                expect_true(is.Datetime(testdf$v5))
            })
            
            test_that("names() are the same and in the right order", {
                expect_identical(names(df), names(testdf))
                with(test.dataset(mrdf), {
                    expect_identical(names(mrdf), names(.setup))
                })
            })
        })
        test_that("Datasets can be deleted", {
            testdf <- newDataset(df, name="delete-me")
            df.name <- name(testdf) ## In case a dataset with this name already existed and the name was munged to be made unique
            expect_true(df.name %in% listDatasets())
            expect_true(DELETE(self(testdf), 
                response.handler=function (response) response$status_code==204))
            expect_false(df.name %in% listDatasets(refresh=TRUE))
            
            ## Do again but with the S4 method
            testdf <- newDataset(df, name="delete-me")
            df.name <- name(testdf)
            delete(testdf)
            expect_false(df.name %in% listDatasets())
        })
    })
}

