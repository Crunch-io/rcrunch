context("Appending subsets")

with_mock_HTTP({
    ds1 <- loadDataset("test ds")
    ds2 <- loadDataset("ECON.sav")
    test_that("If no filtering, 'where' and 'filter' are omitted", {
        expect_POST(appendDataset(ds1, ds2),
            'https://app.crunch.io/api/datasets/1/batches/',
            '{"element":"shoji:entity","body":',
            '{"dataset":"https://app.crunch.io/api/datasets/3/"},',
            '"autorollback":true,"savepoint":true}')
    })
    test_that("Append with filter", {
        expect_POST(appendDataset(ds1, ds2[ds2$gender == "Male",]),
            'https://app.crunch.io/api/datasets/1/batches/',
            '{"element":"shoji:entity","body":',
            '{"dataset":"https://app.crunch.io/api/datasets/3/",',
            '"filter":{"function":"==","args":[{"variable":"https://app.crunch.io/api/datasets/3/variables/gender/"},{"value":1}]}},',
            '"autorollback":true,"savepoint":true}')
    })
    test_that("Append with variable selection", {
        expect_POST(appendDataset(ds1, ds2[c("gender", "birthyr")]),
            'https://app.crunch.io/api/datasets/1/batches/',
            '{"element":"shoji:entity","body":',
            '{"dataset":"https://app.crunch.io/api/datasets/3/",',
            '"where":{"function":"select","args":[{"map":{"66ae9881e3524f7db84970d556c34552":{"variable":"https://app.crunch.io/api/datasets/3/variables/gender/"},"f78ca47313144b57adfb495893968e70":{"variable":"https://app.crunch.io/api/datasets/3/variables/birthyr/"}}}]}},',
            '"autorollback":true,"savepoint":true}')
    })
    test_that("Append with variable selection and filter", {
        expect_POST(appendDataset(ds1, ds2[ds2$gender == "Male", c("gender", "birthyr")]),
            'https://app.crunch.io/api/datasets/1/batches/',
            '{"element":"shoji:entity","body":',
            '{"dataset":"https://app.crunch.io/api/datasets/3/",',
            '"where":{"function":"select","args":[{"map":{"66ae9881e3524f7db84970d556c34552":{"variable":"https://app.crunch.io/api/datasets/3/variables/gender/"},"f78ca47313144b57adfb495893968e70":{"variable":"https://app.crunch.io/api/datasets/3/variables/birthyr/"}}}]},',
            '"filter":{"function":"==","args":[{"variable":"https://app.crunch.io/api/datasets/3/variables/gender/"},{"value":1}]}},',
            '"autorollback":true,"savepoint":true}')
    })
})

with_test_authentication({
    ds1 <- newDataset(df[1:3])
    ds2 <- newDataset(df[2:5])
    test_that("We can select variables to append", {
        ds1 <- appendDataset(ds1, ds2[c("v2", "v5")])
        expect_equal(ncol(ds1), 4)
        expect_identical(names(ds1), c("v1", "v2", "v3", "v5"))
        asdf <- as.data.frame(ds1)
        expect_equivalent(asdf$v1, c(df$v1, rep(NA_real_, 20)))
        expect_equivalent(asdf$v2, c(df$v2, df$v2))
        expect_equivalent(asdf$v3, c(df$v3, rep(NA_real_, 20)))
        expect_equivalent(asdf$v5, c(rep(as.Date(NA), 20), df$v5))
    })
    ds1 <- restoreVersion(ds1, "initial import")
    test_that("We can select rows to append", {
        ds1 <- appendDataset(ds1, ds2[ds2$v3 < 10,])
        expect_equal(ncol(ds1), 5)
        expect_identical(names(ds1), c("v1", "v2", "v3", "v4", "v5"))
        asdf <- as.data.frame(ds1)
        expect_equivalent(asdf$v1, c(df$v1, rep(NA_real_, 2)))
        expect_equivalent(asdf$v2, c(df$v2, df$v2[1:2]))
        expect_equivalent(asdf$v3, c(df$v3, df$v3[1:2]))
    })
    ds1 <- restoreVersion(ds1, "initial import")
    test_that("We can select rows and variables to append", {
        ds1 <- appendDataset(ds1, ds2[ds2$v3 < 10, c("v2", "v5")])
        expect_equal(ncol(ds1), 4)
        expect_identical(names(ds1), c("v1", "v2", "v3", "v5"))
        asdf <- as.data.frame(ds1)
        expect_equivalent(asdf$v1, c(df$v1, rep(NA_real_, 2)))
        expect_equivalent(asdf$v2, c(df$v2, df$v2[1:2]))
        expect_equivalent(asdf$v3, c(df$v3, rep(NA_real_, 2)))
        expect_equivalent(asdf$v5, c(rep(as.Date(NA), 20), df$v5[1:2]))
    })
})
