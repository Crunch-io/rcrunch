context("Hiding variables")

if (!run.only.local.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            testdf <- .setup
            var1 <- testdf[[1]]
            test_that("Hide and unhide method for variables", {
                expect_true(name(var1) %in% findVariables(testdf, key="name", value=TRUE))
                var1 <- hide(var1)
                testdf <- refresh(testdf)
                expect_false(name(var1) %in% findVariables(testdf, key="name", value=TRUE))
                
                var1 <- unhide(var1)
                testdf <- refresh(testdf)
                expect_true(name(var1) %in% findVariables(testdf, key="name", value=TRUE))
            })
        })
        
        with(test.dataset(df), {
            testdf <- .setup
            
            test_that("hideVariables and hiddenVariables for Dataset", {
                expect_equivalent(hidden(testdf)@index, list())
                expect_identical(hiddenVariables(testdf), c())
                expect_identical(dim(testdf), dim(df))
                
                testdf <- hideVariables(testdf, c("v2", "v3"))
                expect_identical(names(testdf)[1:2], c("v1", "v4"))
                expect_identical(hiddenVariables(testdf), c("v2", "v3"))
                expect_identical(length(hidden(testdf)), 2L)
                expect_identical(length(active(testdf@variables)), ncol(df)-2L)
                expect_identical(dim(testdf), c(nrow(df), ncol(df)-2L))
                
                hiddenVariables(testdf) <- "v3"
                ## work like is.na<-, i.e. adds hiding but doesn't unhide by omitting
                expect_identical(hiddenVariables(testdf), c("v2", "v3"))
                expect_identical(names(testdf)[1:2], c("v1", "v4"))
                expect_identical(dim(testdf), c(nrow(df), ncol(df)-2L))
                
                hiddenVariables(testdf) <- "v4"
                expect_identical(names(testdf)[1:2], c("v1", "v5"))
                expect_identical(hiddenVariables(testdf), c("v2", "v3", "v4"))
                expect_identical(dim(testdf), c(nrow(df), ncol(df)-3L))
                
                testdf <- unhideVariables(testdf, c("v2", "v3", "v4"))
                expect_identical(hiddenVariables(testdf), c())
                expect_identical(dim(testdf), dim(df))
            })
        })
        
        with(test.dataset(df), {
            testdf <- .setup
            
            test_that("hideVariables with grep (and by index)", {
                testdf <- hideVariables(testdf, pattern="v[23]")
                # print("ok")
                expect_identical(names(testdf)[1:2], c("v1", "v4"))
                
                testdf <- unhideVariables(testdf, pattern="v[23]")
                # print("ok")
                expect_identical(hiddenVariables(testdf), c())
            })
            
            test_that("Error handling", {
                expect_identical(hiddenVariables(testdf), c()) # To be clear
                ## Need something better than subscript out of bounds, probably
                
            })
        })
    })
}