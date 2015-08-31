context("Exclusion filters")

if (run.integration.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            test_that("We can set and unset an exclusion filter", {
                ds <- refresh(ds)
                expect_identical(zcl(activeFilter(ds)), list())
                expect_identical(nrow(ds), 20L)
                expect_equivalent(as.array(crtabs(~ v4, data=ds)), 
                    array(c(10, 10), dim=2L, dimnames=list(v4=c("B", "C"))))
                expect_identical(exclusion(ds), NULL)
                
                exclusion(ds) <- ds$v4 == "C"
                ## Test that the filter is set correctly. Objects not identical
                ## because JSON objects are unordered.
                e <- zcl(exclusion(ds))
                f <- zcl(ds$v4 == "C")
                expect_identical(e[["function"]], f[["function"]])
                expect_identical(e[["args"]][[1]], f[["args"]][[1]])
                expect_identical(e[["args"]][[2]]$value, f[["args"]][[2]]$value)
                
                expect_identical(nrow(ds), 10L)
                expect_equivalent(as.array(crtabs(~ v4, data=ds)), 
                    array(c(10, 0), dim=2L, dimnames=list(v4=c("B", "C"))))
                
                exclusion(ds) <- NULL
                expect_identical(nrow(ds), 20L)
                expect_equivalent(as.array(crtabs(~ v4, data=ds)), 
                    array(c(10, 10), dim=2L, dimnames=list(v4=c("B", "C"))))
                expect_identical(exclusion(ds), NULL)
            })
            
            test_that("Validation for setting exclusion", {
                expect_error(exclusion(ds) <- "Not a filter", 
                    paste(dQuote("value"), 
                    "must be a CrunchLogicalExpr or NULL, not",
                    dQuote("character")))
            })
        })
        
        with(test.dataset(df), {
            test_that("Exclusion setup", {
                expect_identical(nrow(ds), 20L)
                exclusion(ds) <<- ds$v4 == "C"
                expect_identical(nrow(ds), 10L)
            })
            
            test_that("Add a variable", {
                skip("(400) Bad Request: Problem with input: Cannot insert. Length of cd0bc321f8104743b0baa021f2c0c420 (10) does not match others (20)")
                ds$newvar1 <- 1:10
                expect_identical(as.vector(ds$newvar), 1:10)
                exclusion(ds) <- NULL
                expect_identical(as.vector(ds$newvar),
                    c(1, NA, 2, NA, 3, NA, 4, NA, 5, NA, 6, NA, 7, NA, 8, NA,
                        9, NA, 10, NA))
            })
            
            test_that("Update a variable", {
                skip("(400) Bad Request: Column '40a5355a5d8a4241bd6fbb3b99586951' length 10 does not match existing length 20.")
                exclusion(ds) <<- ds$v4 == "C"
                ds$v3 <- 10:1
                expect_identical(as.vector(ds$v3), 10:1)
                exclusion(ds) <- NULL
                expect_identical(as.vector(ds$v3),
                    c(10, 9, 9, 11, 8, 13, 7, 15, 6, 17, 5, 19, 4, 21, 3, 23,
                        2, 25, 1, 27))
            })
            
            test_that("Update a variable by row index", {
                ## Setup: add a new numeric variable to mess with
                exclusion(ds) <<- NULL
                ds$v3x <- 4
                expect_identical(as.vector(ds$v3x), rep(4, 20))
                
                exclusion(ds) <<- ds$v4 == "C"
                ds$v3x[2] <- 9
                skip("Updates the second row of the un-excluded data, not the excluded data")
                expect_identical(as.vector(ds$v3x), c(4, 9, rep(4, 8)))
                exclusion(ds) <<- NULL
                expect_identical(as.vector(ds$v3x), c(rep(4, 3), 9, rep(4, 16)))
            })
            
            test_that("Append to a dataset", {
                with(test.dataset(df, "part1"), {
                    skip("All rows, not just excluded rows, are appended")
                    exclusion(ds) <<- ds$v4 == "C"
                    out <- appendDataset(part1, ds)
                    expect_identical(nrow(out), 30L)
                    expect_identical(as.vector(out$v4),
                        as.factor(c(rep(LETTERS[2:3], 10), rep("B", 10))))
                })
            })
            
            test_that("Append another dataset to one with exclusion", {
                with(test.dataset(df, "part2"), {
                    exclusion(ds) <<- ds$v4 == "C"
                    out <- appendDataset(ds, part2)
                    expect_identical(nrow(out), 20L)
                    expect_equivalent(as.array(crtabs(~ v4, 
                        data=out)), 
                        array(c(20, 0), dim=2L, dimnames=list(v4=c("B", "C"))))
                })
            })
        })
    })
}