context("Deleting rows of a dataset")

with_mock_HTTP({
    ds <- loadDataset("test ds")
    test_that("dropRows generates the right request", {
        expect_POST(dropRows(ds, ds$gender == "Male"),
            '/api/datasets/dataset1/table/',
            '{"command":"delete","filter":{"function":"==",',
            '"args":[{"variable":"/api/datasets/dataset1/variables/gender/"},',
            '{"value":1}]}}')
    })
})

if (run.integration.tests) {
    with_test_authentication({
        with(test.dataset(df), {
            test_that("dropRows really removes rows", {
                try(ds <- dropRows(ds, ds$v4 == "C"))
                expect_identical(dim(ds), c(10L, ncol(df)))
                expect_identical(as.vector(ds$v4, mode="id"), rep(1, 10))
                expect_identical(as.vector(ds$v3), seq(8, 26, 2))
            })
        })
        with(test.dataset(df), {
            exclusion(ds) <- ds$v4 == "B"
            test_that("dropRows correctly drops with an exclusion applied", {
                expect_identical(nrow(ds), 10L)
                try(ds <- dropRows(ds, ds$v3 > 10 & ds$v3 <= 15))
                expect_identical(dim(ds), c(7L, ncol(df)))
                exclusion(ds) <- NULL
                expect_identical(nrow(ds), 15L)
            })
        })
    })
}
