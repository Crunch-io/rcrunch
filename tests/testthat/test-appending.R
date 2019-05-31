context("Append datasets")

test_that("crunchTimeout", {
    with(
        temp.option(crunch.timeout = 7),
        expect_identical(crunchTimeout(), 7)
    )
    with(
        temp.option(crunch.timeout = NULL),
        expect_identical(crunchTimeout(), 900)
    )
    with(
        temp.option(crunch.timeout = list()),
        expect_identical(crunchTimeout(), 900)
    )
})

with_mock_crunch({
    ds <- loadDataset("test ds")
    test_that("Cannot append dataset to itself", {
        expect_error(
            appendDataset(ds, ds),
            "Cannot append dataset to itself"
        )
    })

    ds1 <- loadDataset("test ds")
    ds2 <- loadDataset("ECON.sav")
    test_that("append DELETEs the pk", {
        expect_DELETE(
            appendDataset(ds2, ds1),
            "https://app.crunch.io/api/datasets/3/pk/"
        )
    })
    test_that("append doesn't DELETE the pk if upsert=TRUE", {
        expect_POST(appendDataset(ds2, ds1, upsert=TRUE),
            "https://app.crunch.io/api/datasets/3/batches/",
            '{"element":"shoji:entity","body":{"dataset":',
            '"https://app.crunch.io/api/datasets/1/"}}'
        )
    })
})


with_test_authentication({
    part1 <- newDataset(df)
    part2 <- newDataset(df)
    cats <- categories(part1$v4)

    ## Set a primary key to test that it gets unset
    pk(part2) <- part2$v3
    v3.1 <- as.vector(part1$v3)
    v3.2 <- as.vector(part2$v3)
    test_that("Setup for appending (mostly) identical datasets", {
        expect_true(is.numeric(v3.1))
        expect_true(is.numeric(v3.2))
        expect_equivalent(v3.1, df$v3)
        expect_equivalent(v3.2, df$v3)
        expect_identical(dim(part1), dim(part2))
        expect_identical(dim(part1), dim(df))
        expect_length(batches(part1), 1)
        expect_length(batches(part2), 1)
        expect_equal(pk(part2), part2$v3)
    })
    out <- appendDataset(part1, part2)
    test_that("append handles two identical Datasets", {
        expect_true(is.dataset(out))
        expect_identical(self(out), self(part1))
        expect_length(batches(out), 2)
        expect_identical(dim(out), c(nrow(df) * 2L, ncol(df)))
        expect_identical(getNrow(out), nrow(df) * 2L)
        expect_identical(nrow(out), length(as.vector(out$v3)))
        expect_identical(categories(out$v4), cats)
        expect_equivalent(as.vector(out$v3), rep(df$v3, 2))
        expect_identical(as.vector(out$v3), c(v3.1, v3.2))
    })
    test_that("append removes the primary key if there is one", {
        expect_null(pk(out))
    })
})
