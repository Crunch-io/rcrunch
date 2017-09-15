context("CrunchDataFrame conformance with data.frame methods")


with_mock_crunch({
    ds <- loadDataset("test ds")

    test.df <- as.data.frame(ds)
    
    test_that("model.frame thus works on CrunchDataset", {
        expect_identical(model.frame(birthyr ~ gender, data=test.df),
                         model.frame(birthyr ~ gender, data=ds))
    })
    
    test_that("so lm() should work too", {
        test.lm <- lm(birthyr ~ gender, data=ds)
        expected <- lm(birthyr ~ gender, data=test.df)
        expect_is(test.lm, "lm")
        expect_identical(names(test.lm), names(expected))
        for (i in setdiff(names(expected), "call")) {
            expect_identical(test.lm[[i]], expected[[i]])
        }
    })
})


with_test_authentication({
    ds <- newDataset(df)
    
    test_that("model.frame thus works on CrunchDataset over API", {
        ## would like this to be "identical" instead of "equivalent"
        expect_equivalent(model.frame(v1 ~ v3, data=ds),
                          model.frame(v1 ~ v3, data=df))
    })
    
    test_that("so lm() should work too over the API", {
        test.lm <- lm(v1 ~ v3, data=ds)
        expected <- lm(v1 ~ v3, data=df)
        expect_is(test.lm, "lm")
        expect_identical(names(test.lm), names(expected))
        ## would like this to be "identical" instead of "equivalent"
        for (i in setdiff(names(expected), "call")) {
            expect_equivalent(test.lm[[i]], expected[[i]])
        }
    })
})