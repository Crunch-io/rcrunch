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
    })
}