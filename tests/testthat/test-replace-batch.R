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
                    d1 <- refresh(d1)
                    expect_identical(nrow(d1), nrow(d2))
                    expect_identical(ncol(d1), ncol(d2)) ## Fails. Because:
                    expect_true(is.null(d1$v6)) ## also fails. Variable still exists
                    expect_identical(as.vector(d1$v3), as.vector(d2$v3))
                    expect_identical(as.vector(d1$v4), as.vector(d2$v4)) ## Fails because categories contain not just "C" and "D" from d2 but "B" from the old d1
                    
                    ## Questions:
                    # 1) If I replace a batch and exclude a variable, should the variable disappear? (Currently, its values are just being NA'd)
                    # 2) If I replace a categorical with different categories, should the categories from the old batch persist? (Currently, the union of categories results)
                })
            })
        })
        
        df3 <- rbind(df, df[1:10,]) ## This one has more rows
        with(test.dataset(df, "d1"), {
            with(test.dataset(df3, "d2"), {
                d1$derivedvar <- d1$v3 + 9 ## Add a derived var. 
                d1$v3[d1$v3 <= 10] <- 11 ## Functionally modify some values
                names(categories(d1$v4)) <- c("bee", "see") ## Rename cats
                out <- try(replaceBatch(d1, d2))
                ## ^ fails with Error : server error: (500) Internal Server Error: No variable with id '664d77c95fc84636bc5388fe5240ba91' (or whatever UUID)
                test_that("Edits to the dataset persist when replacing data", {
                    expect_true(is.dataset(out))
                    if (is.dataset(out)) {
                        ## No sense in running these if not
                        expect_identical(ncol(out), ncol(d2) + 1L)
                        expect_identical(nrow(out), nrow(d2))
                        expect_identical(as.vector(out$derivedvar), 
                            c(20, 20, 20, 20, 21, 22, 23, 24, 25, 26,
                              27, 28, 29, 30, 31, 32, 33, 34, 35, 36,
                              20, 20, 20, 20, 21, 22, 23, 24, 25, 26))
                        expect_identical(names(categories(out)),
                            names(categories(d1)))
                    }
                })
            })
        })
    })
}