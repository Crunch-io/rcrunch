context("Deleting rows of a dataset")

with_mock_crunch({
    ds <- loadDataset("test ds")
    test_that("dropRows generates the right request", {
        expect_POST(
            dropRows(ds, ds$gender == "Male"),
            "https://app.crunch.io/api/datasets/1/table/",
            '{"command":"delete","filter":{"function":"==",',
            '"args":[{"variable":"https://app.crunch.io/api/datasets/1/variables/gender/"},',
            '{"value":1}]}}'
        )
    })
    test_that("dropRows doesn't send invalid expressions", {
        expect_error(dropRows(ds, ds$NOTAVARIABLE == "Male"),
            'Invalid expression: ds$NOTAVARIABLE == "Male"',
            fixed = TRUE
        )
        expect_error(
            dropRows(ds, NULL),
            "Invalid expression: NULL"
        )
    })
})

with_test_authentication({
    test_that("dropRows really removes rows", {
        ds1 <- newDataset(df)
        ds1 <- dropRows(ds1, ds1$v4 == "C")
        expect_identical(dim(ds1), c(10L, ncol(df)))
        expect_identical(as.vector(ds1$v4, mode = "id"), rep(1, 10))
        expect_identical(as.vector(ds1$v3), seq(8, 26, 2))
    })

    test_that("dropRows correctly drops with an exclusion applied", {
        ds2 <- newDataset(df)
        exclusion(ds2) <- ds2$v4 == "B"
        expect_identical(nrow(ds2), 10L)
        ds2 <- dropRows(ds2, ds2$v3 > 10 & ds2$v3 <= 15)
        expect_identical(dim(ds2), c(7L, ncol(df)))
        exclusion(ds2) <- NULL
        expect_identical(nrow(ds2), 15L)
    })
})
