context("Making a new dataset")

test_that("fake.csv is what we expect", {
    expect_identical(dim(testfile.df), c(20L, 6L))
})

if (run.integration.tests) {
    test_that("Source file cannot be uploaded if not logged in", {
        logout()
        expect_error(createSource(testfile.csv), 
            "You must authenticate before making this request")
    })
    test_that("Dataset container object cannot be created if not logged in", {
        logout()
        expect_error(createDataset("testfile.csv"), 
            "You must authenticate before making this request")
    })
        
    with(test.authentication, {
        ## New dataset by file upload method
        test_that("Source file can be uploaded if logged in", {
            expect_true(createSource(testfile.csv, 
                response.handler=function (response) response$status_code==201))
        })
        test_that("Dataset container object can be created if logged in", {
            with(test.dataset(), {
                expect_true(is.dataset(ds))
            })
        })
        test_that("Source can be added to Dataset", {
            source <- createSource(testfile.csv)
            with(test.dataset(), {
                ds <- try(addSourceToDataset(ds, source))
                expect_true(is.dataset(ds))
                expect_identical(nrow(ds), 20L)
                expect_identical(ncol(ds), 6L)
            })
        })
        test_that("newDatasetFromFile creates a dataset", {
            ds <- newDatasetFromFile(testfile.csv, name=uniqueDatasetName())
                expect_true(is.dataset(ds))
                expect_identical(nrow(ds), 20L)
                expect_identical(ncol(ds), 6L)
                expect_equivalent(mean(ds[[2]]), mean(testfile.df[[2]]))
            delete(ds)
        })
        test_that("Dataset can be made from a data.frame by dumping a csv", {
            dsname <- uniqueDatasetName()
            d1 <- newDatasetViaFile(df, name=dsname)
                expect_true(dsname %in% listDatasets())
            delete(d1)
            
            testcrdf <- newDatasetViaFile(df, name=uniqueDatasetName())
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
            dsname <- uniqueDatasetName()
            dx <- try(newDataset(df, name=dsname, description="a description"))
                expect_true(dsname %in% listDatasets())
                expect_true(is.dataset(dx))
                expect_identical(description(dx), "a description")
                expect_equivalent(mean(dx$v3), mean(df$v3))
                expect_identical(dim(dx), dim(df))
            try(delete(dx))
        })
        
        test_that("newDataset(FromFile) passes useAlias", {
            d1 <- newDataset(df, name=uniqueDatasetName())
                expect_equal(d1@useAlias, default.useAlias())
            delete(d1)
            d1 <- newDataset(df, name=uniqueDatasetName(), useAlias=FALSE)
                expect_false(d1@useAlias)
            delete(d1)
        })
        with(test.dataset(df, "testdf"), {
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
            
            with(test.dataset(mrdf, "testmrdf"), {
                test_that("names() are the same and in the right order", {
                    expect_true(setequal(names(df), names(testdf)))
                    expect_identical(names(df), names(testdf))
                    expect_true(setequal(names(mrdf), names(testmrdf)))
                    expect_identical(names(mrdf), names(testmrdf))
                })
            })
        })
        test_that("Datasets can be deleted", {
            dsname <- uniqueDatasetName()
            testdf <- newDataset(df, name=dsname)
            expect_true(dsname %in% listDatasets())
            expect_true(DELETE(self(testdf), 
                response.handler=function (response) response$status_code==204))
            expect_false(dsname %in% listDatasets(refresh=TRUE))
            
            ## Do again but with the S4 method
            dsname <- uniqueDatasetName()
            testdf <- newDataset(df, name=dsname)
            expect_true(dsname %in% listDatasets())
            delete(testdf)
            expect_false(dsname %in% listDatasets())
        })
        
    })
}

