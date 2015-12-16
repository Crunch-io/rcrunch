context("Replace data")

if (run.integration.tests) {
    with(test.authentication, {
        df2 <- df[1:12,]  ## Take fewer rows
        df2$v3 <- sample(df2$v3, nrow(df2), replace=FALSE) + 9 ## Change values
        df2$v4 <- as.factor(LETTERS[3:4]) ## Redefine categories
        df2$v6 <- NULL ## Drop a variable
        with(test.dataset(df, "d1"), {
            with(test.dataset(df2, "d2"), {
                test_that("The two datasets are different", {
                    expect_false(nrow(d1) == nrow(d2))
                    expect_false(ncol(d1) == ncol(d2))
                    expect_false(all(levels(as.vector(d1$v4)) == levels(as.vector(d2$v4))))
                    expect_false(is.null(d1$v6))
                })
                out <- try(replaceBatch(d1, d2))
                test_that("A very similar batch can be replaced", {
                    expect_true(is.dataset(out))
                    expect_identical(self(out), self(d1))
                    expect_identical(nrow(out), nrow(d2))
                })
                test_that("Variables with no data in the new batch still exist", {
                    expect_identical(ncol(d1), ncol(df))
                    ## Equal because:
                    expect_false(is.null(d1$v6))
                    ## Variable still exists bc defined in original metadata. 
                    expect_true(all(is.na(as.vector(d1$v6))))
                    ## But it has no values bc it was excluded in new dataset
                    expect_identical(as.vector(d1$v3), as.vector(d2$v3))
                })
                test_that("If categories are added in the new data, the union of categories is kept", {
                    expect_identical(sort(names(categories(d1$v4))), 
                        c("B", "C", "D", "No Data")) 
                        ## Categories contain not just "C" and "D" from d2 but
                        ## "B" from the old d1. Add the sort() because No Data
                        ## isn't last (though it probably should be)
                })
            })
        })
        
        df3 <- rbind(df, df[1:10,]) ## This one has more rows
        with(test.dataset(df, "d1"), {
            with(test.dataset(df3, "d2"), {
                d1$derivedvar <- d1$v3 + 9 ## Add a derived var. 
                d1$v3[d1$v3 <= 10] <- 11 ## Functionally modify some values
                names(categories(d1$v4))[1:2] <- c("bee", "see") ## Rename cats
                name(d1$v1) <- "First variable"
                out <- try(replaceBatch(d1, d2))
                test_that("Replacing data when the dataset has been edited", {
                    expect_true(is.dataset(out))
                    expect_identical(nrow(out), nrow(d2))
                })
                test_that("The derived variable still exists", {
                    expect_identical(ncol(out), ncol(d2) + 1L)
                    expect_identical(as.vector(out$derivedvar), 
                        c(17, 18, 19, 20, 21, 22, 23, 24, 25, 26,
                          27, 28, 29, 30, 31, 32, 33, 34, 35, 36,
                          17, 18, 19, 20, 21, 22, 23, 24, 25, 26))
                        ## Update of values is not an action that is
                        ## replayed. If it were, the expected values would
                        ## be:
                        # c(20, 20, 20, 20, 21, 22, 23, 24, 25, 26,
                        #   27, 28, 29, 30, 31, 32, 33, 34, 35, 36,
                        #   20, 20, 20, 20, 21, 22, 23, 24, 25, 26))
                })
                test_that("The edited variable name persists", {    
                    expect_identical(name(out$v1), "First variable")
                })
                test_that("The category name edits map correctly", {
                    skip("Categories actually are bee, see, ND, B, C")
                    expect_identical(names(categories(out$v4)),
                        c("bee", "see", "No Data"))
                })
            })
        })
    })
}