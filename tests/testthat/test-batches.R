context("Batch catalog")

with_mock_crunch({
    ds <- cachedLoadDataset("test ds")
    test_that("batches method", {
        expect_is(batches(ds), "BatchCatalog")
        expect_length(batches(ds), 3)
        expect_identical(
            urls(batches(ds)),
            c(
                "https://app.crunch.io/api/datasets/1/batches/0/",
                "https://app.crunch.io/api/datasets/1/batches/2/",
                "https://app.crunch.io/api/datasets/1/batches/3/"
            )
        )
    })

    test_that("imported/pending", {
        expect_identical(
            urls(imported(batches(ds))),
            c(
                "https://app.crunch.io/api/datasets/1/batches/0/",
                "https://app.crunch.io/api/datasets/1/batches/2/"
            )
        )
        expect_identical(
            urls(pending(batches(ds))),
            "https://app.crunch.io/api/datasets/1/batches/3/"
        )
    })

    test_that("as.data.frame method for batch catalog", {
        expect_identical(
            as.data.frame(batches(ds)),
            data.frame(
                id = c(0L, 2L, 3L),
                status = c("imported", "imported", "conflict"),
                stringsAsFactors = FALSE
            )
        )
    })
    test_that("show method for batch catalog", {
        expect_prints(
            batches(ds),
            get_output(data.frame(
                id = c(0, 2, 3),
                status = c("imported", "imported", "conflict")
            ))
        )
    })
})
