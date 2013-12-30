context("Hiding variables")

if (!run.only.local.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            testdf <- .setup
            var1 <- testdf[[1]]
            test_that("Hide and unhide method for variables", {
                expect_true(name(var1) %in% vapply(testdf[], 
                    function (x) name(x), character(1)))
                var1 <- hide(var1)
                testdf <- refresh(testdf)
                expect_false(name(var1) %in% vapply(testdf[], 
                    function (x) name(x), character(1)))
                
                var1 <- unhide(var1)
                testdf <- refresh(testdf)
                expect_true(name(var1) %in% vapply(testdf[], 
                    function (x) name(x), character(1)))
            })
        })
        
        with(test.dataset(df), {
            testdf <- .setup
            
            test_that("hideVariables and hiddenVariables for Dataset", {
                expect_identical(list(),
                    getShojiCollection(testdf@urls$discarded_variables_url))
                expect_identical(list(),
                    getShojiCollection(testdf@urls$discarded_variables_url,
                    "alias"))
                expect_identical(hiddenVariablesList(testdf), list())
                expect_identical(hiddenVariables(testdf), c())
                
                testdf <- hideVariables(testdf, c("v2", "v3"))
                expect_identical(names(testdf)[1:2], c("v1", "v4"))
                
                expect_identical(names(hiddenVariablesList(testdf)), 
                    c("v2", "v3"))
                expect_true(all(vapply(hiddenVariablesList(testdf), is.variable,
                    logical(1))))
                expect_identical(hiddenVariables(testdf), c(v2="v2", v3="v3"))
                
                hiddenVariables(testdf) <- "v3"
                ## work like is.na<-, i.e. adds hiding but doesn't unhide by omitting
                expect_identical(hiddenVariables(testdf), c(v2="v2", v3="v3"))
                expect_identical(names(testdf)[1:2], c("v1", "v4"))
                hiddenVariables(testdf) <- "v4"
                expect_identical(names(testdf)[1:2], c("v1", "v5"))
                
                testdf <- unhideVariables(testdf, c("v2", "v3", "v4"))
                expect_identical(hiddenVariables(testdf), c())
            })
        })
        
        with(test.dataset(df), {
            testdf <- .setup
            
            test_that("hideVariables with grep (and by index)", {
                testdf <- hideVariables(testdf, pattern="v[23]")
                expect_identical(names(testdf)[1:2], c("v1", "v4"))
                
                testdf <- unhideVariables(testdf, pattern="v[23]")
                expect_identical(hiddenVariables(testdf), c())
            })
            
            test_that("Error handling", {
                expect_identical(hiddenVariables(testdf), c()) # To be clear
                ## Need something better than subscript out of bounds, probably
                
            })
        })
    })
}