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
            with(test.dataset(newDatasetFromFile(testfile.csv,
                                                name=uniqueDatasetName())), {
                expect_true(is.dataset(ds))
                expect_identical(nrow(ds), 20L)
                expect_identical(ncol(ds), 6L)
                expect_equivalent(mean(ds[[2]]), mean(testfile.df[[2]]))
            })
        })

        test_that("newDataset input validation", {
            expect_error(newDataset(NULL), 
                "Can only make a Crunch dataset from a two-dimensional data")
            expect_error(newDataset(1:5), 
                "Can only make a Crunch dataset from a two-dimensional data")
        })
        
        test_that("newDataset by addVariables", {
            dsname <- uniqueDatasetName()
            with(test.dataset(newDataset(df, name=dsname,
                                        description="a description")), {
                expect_true(dsname %in% listDatasets())
                expect_true(is.dataset(ds))
                expect_identical(description(ds), "a description")
                expect_equivalent(mean(ds$v3), mean(df$v3))
                expect_identical(dim(ds), dim(df))
            })
        })
        
        test_that("newDataset passes useAlias", {
            with(test.dataset(newDataset(df, name=uniqueDatasetName())), 
                expect_equal(ds@useAlias, default.useAlias())
            )
            with(test.dataset(newDataset(df, name=uniqueDatasetName(),
                                        useAlias=FALSE)),
                expect_false(ds@useAlias)
            )
        })
        with(test.dataset(df), {
            test_that("Dataset variable types get set correctly", {
                expect_true(is.Numeric(ds[["v1"]]))
                expect_true(is.Text(ds[["v2"]]))
                expect_true(is.Numeric(ds[["v3"]]))
                expect_true(is.Categorical(ds[["v4"]]))
                expect_true(all(levels(df$v4) %in% names(categories(ds$v4))))
                expect_identical(categories(ds$v4),
                    categories(refresh(ds$v4)))
                expect_identical(ds$v4, refresh(ds$v4))
                expect_true(is.Datetime(ds$v5))
                expect_true(is.Categorical(ds$v6))
            })
            
            with(test.dataset(mrdf, "testmrdf"), {
                test_that("names() are the same and in the right order", {
                    expect_true(setequal(names(df), names(ds)))
                    expect_identical(names(df), names(ds))
                    expect_true(setequal(names(mrdf), names(testmrdf)))
                    expect_identical(names(mrdf), names(testmrdf))
                })
            })
        })
                
        test_that("newDataset via CSV + JSON", {
            with(test.dataset(suppressMessages(newDataset2(df,
                                                name=uniqueDatasetName()))), {
                expect_true(is.dataset(ds))
                expect_identical(names(df), names(ds))
                expect_identical(dim(ds), dim(df))
                expect_true(is.Numeric(ds[["v1"]]))
                expect_true(is.Text(ds[["v2"]]))
                expect_true(is.Numeric(ds[["v3"]]))
                expect_equivalent(as.array(crtabs(mean(v3) ~ v4, data=ds)),
                    tapply(df$v3, df$v4, mean, na.rm=TRUE))
                expect_true(is.Categorical(ds[["v4"]]))
                expect_equivalent(as.array(crtabs(~ v4, data=ds)), 
                    array(c(10, 10), dim=2L, dimnames=list(v4=c("B", "C"))))
                expect_true(all(levels(df$v4) %in% names(categories(ds$v4))))
                expect_identical(categories(ds$v4),
                    categories(refresh(ds$v4)))
                expect_identical(ds$v4, refresh(ds$v4))
                expect_true(is.Datetime(ds$v5))
                expect_true(is.Categorical(ds$v6))
            })
        })
        
        test_that("Datasets can be deleted", {
            dsname <- uniqueDatasetName()
            testdf <- newDataset(df, name=dsname)
            expect_true(dsname %in% listDatasets())
            expect_true(crDELETE(self(testdf), 
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

