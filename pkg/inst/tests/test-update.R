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
                skip({
                    try(ds$v3[1:10] <- 2)
                    expect_equivalent(mean(ds$v3), 1.5)
                }, "How do I create filter with numeric indices?")
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
                newvals <- as.Date(0:12, origin="1985-10-26")
                try(ds$v5[ds$v5 >= as.Date("1955-11-12")] <- newvals)
                expect_identical(max(ds$v5), as.POSIXct("1985-11-07"))
            })
            
            test_that("Can update categorical variables", {
                try(ds$v4[is.na(ds$v2)] <- "B")
                expect_identical(table(ds$v4)["B"], c(B=13L))
                try(ds$v4[is.na(ds$v2)] <- factor("C"))
                expect_identical(table(ds$v4)["C"], c(C=12L))
                try(ds$v4[is.na(ds$v2)] <- c(2,1,2,1,2))
                expect_equivalent(table(ds$v4), table(df$v4))
                expect_error(ds$v4[is.na(ds$v2)] <- as.factor(LETTERS[1:5]),
                    "Input values A, D, and E are not present in the category names of variable")
            })
            
            skip({
            test_that("Can update with missing values", {
                try(ds$v4[is.na(ds$v2)] <- NA)
                expect_identical(as.vector(ds$v4),
                    as.factor(c(rep(LETTERS[2:3], length=15), rep(NA, 5))))
                try(ds$v1[is.na(ds$v4)] <- NA)
                expect_identical(sum(is.na(as.vector(ds$v1))), 10L)
            })
            
            test_that("Can 'mark missing'", {
                try(is.na(ds$v5) <- ds$v4 == "B")
                try(is.na(ds$v6) <- ds$v4 %in% "C")
            })
            })
        })
        
        with(test.dataset(mrdf), {
            ds <- mrdf.setup(ds)
            test_that("Can update array subvariables", {
                
            })
        })
    })
}