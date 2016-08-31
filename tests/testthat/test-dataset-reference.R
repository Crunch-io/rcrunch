context("datasetReference")

with_mock_HTTP({
    ds <- loadDataset("test ds")
    ds_url <- self(ds)
    test_that("datasetReference methods", {
        expect_identical(ds_url, "/api/datasets/dataset1/")
        expect_identical(datasetReference(ds), ds_url)
        expect_identical(datasetReference(ds$gender), ds_url)
        expect_identical(datasetReference(ds$mymrset$subvar1), ds_url)
        expect_identical(datasetReference(ds$gender == "Male"), ds_url)
        expect_null(datasetReference(1))
        expect_identical(datasetReference("/api/datasets/dataset1/variables/mymrset/subvariables/subvar1"), ds_url)
    })
})
