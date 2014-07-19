context("Update a dataset")

## For functional transformations, distinguish between "do this update to fix this data once" and "do this to every batch I append too"?

if (!run.only.local.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            ds <- .setup
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
                skip(try(ds[ds$v3 == 2, "v3"] <- 4),
                    "do this later")
                ## instead:
                try(ds$v3[ds$v3 == 2] <- 4)
                expect_equivalent(mean(ds$v3), 3.5)
                try(ds$v3[] <- c(rep(5, 10), rep(7, 10)))
                expect_equivalent(mean(ds$v3), 6)
            })
            
            test_that("Can update numeric variable with expresssion", {
                try(ds$v3 <- ds$v3 + 2)
                expect_equivalent(as.vector(ds$v3), c(rep(7, 10), rep(9, 10)))
            })
        })
    })
}