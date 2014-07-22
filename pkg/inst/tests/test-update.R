context("Update a dataset")

## For functional transformations, distinguish between "do this update to fix this data once" and "do this to every batch I append too"?

if (!run.only.local.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            test_that("Can update numeric variable with values", {
                try(ds$v3 <- 9:28)
                test <- as.vector(ds$v3) - df$v3
                expect_true(all(test == 1))
            })
            
            test_that("Value recycling on insert is consistent with R", {
                try(ds$v3 <- 1)
                expect_true(all(as.vector(ds$v3) == 1))
            })
            
            test_that("Can update numeric variable with filter and values", {
                try(ds$v3[1:10] <- 2)
                skip(expect_equivalent(mean(ds$v3), 1.5),
                    "How do I create filter with numeric indices?")
                ds$v3 <- c(rep(2, 10), rep(1, 10))
                expect_equivalent(mean(ds$v3), 1.5)
                try(ds$v3[ds$v3 == 1] <- 3)
                expect_equivalent(mean(ds$v3), 2.5)
                try(ds[ds$v3 == 2, "v3"] <- 4)
                expect_equivalent(mean(ds$v3), 3.5)
                try(ds$v3[] <- c(rep(5, 10), rep(7, 10)))
                expect_equivalent(mean(ds$v3), 6)
            })
            
            test_that("Can update numeric variable with expresssion", {
                try(ds$v3 <- ds$v3 + 2)
                expect_equivalent(as.vector(ds$v3), c(rep(7, 10), rep(9, 10)))
            })
            
            test_that("Can filter on is.na", {
                try(ds$v3[is.na(ds$v2)] <- 0)
                expect_equivalent(as.vector(ds$v3), 
                    c(rep(7, 10), rep(9, 5), rep(0, 5)))
            })
            
            test_that("Can update text", {
                try(ds$v2[is.na(ds$v1)] <- "z")
                expect_identical(as.vector(ds$v2)[1:8], 
                    c(rep("z", 5), "f", "g", "h"))
                try(ds[ds$v2 %in% "z", "v2"] <- "y")
                expect_identical(as.vector(ds$v2)[1:8], 
                    c(rep("y", 5), "f", "g", "h"))
            })
            
            test_that("Can update datetime", {
                
            })
            
            skip(test_that("Can update categorical variables", {
                try(ds$v4[is.na(ds$v2)] <- "B")
                expect_identical(table(ds$v4)["B"], 13L)
            }))
            
            test_that("Can update with missing values", {
                
            })
            
            test_that("Can 'mark missing'", {
                
            })
        })
        
        with(test.dataset(mrdf), {
            test_that("Can update array subvariables", {
                
            })
        })
    })
}