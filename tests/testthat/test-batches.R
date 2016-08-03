context("Batch catalog")

with_mock_HTTP({
    ds <- loadDataset("test ds")
    test_that("batches method", {
        expect_is(batches(ds), "BatchCatalog")
        expect_length(batches(ds), 3)
        expect_identical(urls(batches(ds)),
            c("/api/datasets/dataset1/batches/0/",
            "/api/datasets/dataset1/batches/2/",
            "/api/datasets/dataset1/batches/3/"))
    })

    test_that("imported/pending", {
        expect_identical(urls(imported(batches(ds))),
            c("/api/datasets/dataset1/batches/0/",
            "/api/datasets/dataset1/batches/2/"))
        expect_identical(urls(pending(batches(ds))),
            "/api/datasets/dataset1/batches/3/")
    })

    test_that("show method for batch catalog", {
        expect_output(batches(ds),
            get_output(data.frame(id=c(0, 2, 3),
            status=c("imported", "imported", "conflict"))))
    })
})
