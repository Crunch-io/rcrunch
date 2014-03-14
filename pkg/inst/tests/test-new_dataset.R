context("Making a new dataset")

testfile <- system.file("fake.csv", package="rcrunch", mustWork=TRUE)
if (!run.only.local.tests) {
    test_that("Source file cannot be uploaded if not logged in", {
        logout()
        expect_error(createSource(testfile), 
            "You must authenticate before making this request")
    })
    test_that("Dataset container object cannot be created if not logged in", {
        logout()
        expect_error(createDataset("testfile"), 
            "You must authenticate before making this request")
    })
        
    with(test.authentication, {
        ## New dataset by file upload method
        test_that("Source file can be uploaded if logged in", {
            expect_true(createSource(testfile, 
                response.handler=function (response) response$status_code==201))
        })
        test_that("Dataset container object can be created if logged in", {
            with(test.dataset(), {
                expect_true(is.dataset(.setup))
            })
        })
        test_that("Source can be added to Dataset", {
            source <- createSource(testfile)
            with(test.dataset(), {
                ds <- .setup
                expect_true(is.dataset(addSourceToDataset(ds, source)))
            })
        })
        test_that("newDatasetFromFile creates a dataset", {
            df <- newDatasetFromFile(testfile)
                expect_true(is.dataset(df))
                localdf <- read.csv(testfile)
                expect_equivalent(mean(df[[2]]), mean(localdf[[2]]))
            delete(df)
        })
        test_that("Dataset can be made from a data.frame by dumping a csv", {
            d1 <- newDatasetViaFile(df, name="making_a_dataset_from_df")
                expect_true("making_a_dataset_from_df" %in% listDatasets())
            delete(d1)
            
            testcrdf <- newDatasetViaFile(df)
                expect_true(is.dataset(testcrdf))
            delete(testcrdf)
            ## Should also test doing this with a matrix
        })
        
        ## New dataset by add variable API
        test_that("newDataset by addVariables", {
            expect_error(newDataset(NULL), 
                "Can only make a Crunch dataset from a two-dimensional data")
            expect_error(newDataset(1:5), 
                "Can only make a Crunch dataset from a two-dimensional data")
            dx <- try(newDataset(df, "addVariables", description="a description"))
                expect_true("addVariables" %in% listDatasets())
                expect_true(is.dataset(dx))
                expect_identical(description(dx), "a description")
                expect_equivalent(mean(dx$v3), mean(df$v3))
                expect_identical(dim(dx), dim(df))
            try(delete(dx))
        })
        
        test_that("newDataset(FromFile) passes useAlias", {
            d1 <- newDataset(df)
                expect_equal(d1@useAlias, default.useAlias())
            delete(d1)
            d1 <- newDataset(df, useAlias=FALSE)
                expect_false(d1@useAlias)
            delete(d1)
        })
        with(test.dataset(df), {
            testdf <- .setup
            test_that("Dataset variable types get set correctly", {
                expect_true(is.Numeric(testdf[["v1"]]))
                expect_true(is.Text(testdf[["v2"]]))
                expect_true(is.Numeric(testdf[["v3"]]))
                expect_true(is.Categorical(testdf[["v4"]]))
                expect_true(all(levels(df$v4) %in% names(categories(testdf$v4))))
                expect_identical(categories(testdf$v4),
                    categories(refresh(testdf$v4)))
                expect_identical(testdf$v4, refresh(testdf$v4))
                expect_true(is.Datetime(testdf$v5))
                expect_true(is.Categorical(testdf$v6))
            })
            
            with(test.dataset(mrdf), {
                testmrdf <- .setup
                test_that("names() are the same and in the right order", {
                    expect_true(setequal(names(df), names(testdf)))
                    expect_identical(names(df), names(testdf))
                    expect_true(setequal(names(mrdf), names(testmrdf)))
                    expect_identical(names(mrdf), names(testmrdf))
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

