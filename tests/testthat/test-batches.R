context("Batch catalog")

with_mock_crunch({
    ds <- loadDataset("test ds")
    test_that("batches method", {
        expect_is(batches(ds), "BatchCatalog")
        expect_length(batches(ds), 3)
        expect_identical(urls(batches(ds)),
            c("https://app.crunch.io/api/datasets/1/batches/0/",
            "https://app.crunch.io/api/datasets/1/batches/2/",
            "https://app.crunch.io/api/datasets/1/batches/3/"))
    })

    test_that("imported/pending", {
        expect_identical(urls(imported(batches(ds))),
            c("https://app.crunch.io/api/datasets/1/batches/0/",
            "https://app.crunch.io/api/datasets/1/batches/2/"))
        expect_identical(urls(pending(batches(ds))),
            "https://app.crunch.io/api/datasets/1/batches/3/")
    })

    test_that("show method for batch catalog", {
        expect_output(batches(ds),
            get_output(data.frame(id=c(0, 2, 3),
            status=c("imported", "imported", "conflict"))))
    })
})
