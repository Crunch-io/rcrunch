context("Subvariables")

with(fake.HTTP, {
    session_store$datasets <- DatasetCatalog(GET("api/datasets.json"))
    test.ds <- loadDataset("test ds")
    mr <- test.ds$mymrset
    
    test_that("setup", {
        expect_true(is.Multiple(mr))
    })
    
    test_that("subvariables are what we think", {
        expect_true(inherits(subvariables(mr), "Subvariables"))
        expect_identical(names(subvariables(mr)), c("First", "Second", "Last"))
    })
})

if (!run.only.local.tests) {
    with(test.authentication, {
        with(test.dataset(mrdf), {
            testdf <- .setup
            cast.these <- grep("mr_", names(testdf))
            testdf[cast.these] <- lapply(testdf[cast.these],
                castVariable, "categorical")
            var <- makeMR(pattern="mr_[123]", dataset=testdf,
                name="test1", selections="1.0")
            
            test_that("setup test case 2", {
                expect_true(is.Multiple(var))
                expect_identical(names(subvariables(var)),
                    c("mr_1", "mr_2", "mr_3"))
            })
            
            test_that("can rename subvariables", {
                try(names(subvariables(var))[2] <- "M.R. Two")
                expect_identical(names(subvariables(var)),
                    c("mr_1", "M.R. Two", "mr_3"))
            })
            test_that("can reorder subvariables", {
                try(subvariables(var) <- subvariables(var)[c(3,1,2)])
                expect_identical(names(subvariables(var)),
                    c("mr_3", "mr_1", "M.R. Two"))
            })
            test_that("can't (yet) otherwise modify subvariables", {
                expect_error(subvariables(var) <- NULL)
                expect_error(subvariables(var) <- Subvariables())
            })
        })
    })
}