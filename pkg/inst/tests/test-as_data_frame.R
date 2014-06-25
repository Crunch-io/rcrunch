context("Getting values to make local R objects")


with(fake.HTTP, {
    session_store$datasets <- DatasetCatalog(GET("api/datasets.json"))
    test.ds <- loadDataset("test ds")
    # test.ds <- as.dataset(GET("api/datasets/dataset1.json"))
    
    hiddenVariables(test.ds) <- "mymrset" # Defer implementing MR as.vector
    test_that("setup", {
        expect_identical(dim(test.ds), c(nrow(test.ds), ncol(test.ds)))
        expect_identical(dim(test.ds), c(25L, 4L))
        expect_identical(names(test.ds),
            c("birthyr", "gender", "textVar", "starttime"))
    })
    
    test_that("as.vector on Variables", {
        expect_true(is.numeric(getValues(test.ds$birthyr)))
        expect_false(is.factor(getValues(test.ds$gender)))
        expect_true(is.factor(as.vector(test.ds$gender)))
        expect_true(all(levels(as.vector(test.ds$gender)) %in% names(categories(test.ds$gender))))
    })

    test_that("as.data.frame on CrunchDataset", {
        expect_true(is.data.frame(as.data.frame(test.ds)))
        expect_identical(dim(as.data.frame(test.ds)), c(25L, ncol(test.ds)))
        expect_identical(names(as.data.frame(test.ds)), names(test.ds))
        expect_identical(as.data.frame(test.ds)$birthyr, as.vector(test.ds$birthyr))
    })
    
    test.df <- as.data.frame(test.ds)
    
    test_that("model.frame thus works on CrunchDataset", {
        expect_identical(model.frame(birthyr ~ gender, data=test.df),
            model.frame(birthyr ~ gender, data=test.ds))
    })
    
    test_that("so lm() should work too", {
        test.lm <- lm(birthyr ~ gender, data=test.ds)
        expected <- lm(birthyr ~ gender, data=test.df)
        expect_true(inherits(test.lm, "lm"))
        expect_identical(names(test.lm), names(expected))
        for (i in setdiff(names(expected), "call")) {
            expect_identical(test.lm[[i]], expected[[i]])
        }
    })
})

if (!run.only.local.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            test.asdf <- .setup
        
            test_that("as.vector methods correctly handle data from the API", {
                expect_true(is.Numeric(test.asdf[["v1"]]))
                expect_true(is.Text(test.asdf[["v2"]]))
                expect_true(is.Numeric(test.asdf[["v3"]]))
                expect_true(is.Categorical(test.asdf[["v4"]]))
                expect_true(is.Datetime(test.asdf$v5))
        
                expect_true(is.numeric(as.vector(test.asdf$v1)))
                expect_identical(sum(is.na(as.vector(test.asdf$v1))), 5L)
                expect_equivalent(as.vector(test.asdf$v1), df$v1)
        
                expect_true(is.character(as.vector(test.asdf$v2)))
                expect_identical(sum(is.na(as.vector(test.asdf$v2))), 5L)
                expect_equivalent(as.vector(test.asdf$v2), df$v2)
        
                expect_true(is.numeric(as.vector(test.asdf$v3)))
                expect_equivalent(as.vector(test.asdf$v3), df$v3)
                
                expect_true(is.factor(as.vector(test.asdf$v4)))
                expect_equivalent(as.vector(test.asdf$v4), df$v4)
                
                expect_true(inherits(as.vector(test.asdf$v5), "POSIXt"))
                expect_equivalent(as.vector(test.asdf$v5),
                    as.POSIXct(as.character(df$v5)))
            })
        
            test_that("as.data.frame with API", {
                expect_true(is.data.frame(as.data.frame(test.asdf)))
                expect_identical(dim(as.data.frame(test.asdf)), dim(df))
                expect_identical(names(as.data.frame(test.asdf)), names(df))
                expect_identical(as.data.frame(test.asdf)$v1,
                    as.vector(test.asdf$v1))
            })
        
            test_that("model.frame thus works on CrunchDataset over API", {
                ## would like this to be "identical" instead of "equivalent"
                expect_equivalent(model.frame(v1 ~ v3, data=test.asdf),
                    model.frame(v1 ~ v3, data=df))
            })
        
            test_that("so lm() should work too over the API", {
                test.lm <- lm(v1 ~ v3, data=test.asdf)
                expected <- lm(v1 ~ v3, data=df)
                expect_true(inherits(test.lm, "lm"))
                expect_identical(names(test.lm), names(expected))
                ## would like this to be "identical" instead of "equivalent"
                for (i in setdiff(names(expected), "call")) {
                    expect_equivalent(test.lm[[i]], expected[[i]])
                }
            })
        })
    })
}

