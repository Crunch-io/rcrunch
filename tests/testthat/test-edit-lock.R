context("Locking and unlocking edit privileges")

with_test_authentication({
    ds <- newDataset(df[,4,drop=FALSE])
    test_that("I can lock the dataset", {
        lock(ds)
    })
    test_that("When the dataset is locked, I can edit dataset metadata", {
        expect_error(name(ds) <- "Locked name",
            NA)
        expect_identical(name(ds), "Locked name")
    })
    test_that("When the dataset is locked, I can't edit variables", {
        expect_error(categories(ds$v4) <- rev(categories(ds$v4)),
            "unlock")
        expect_identical(names(categories(ds$v4)), c("B", "C", "No Data"))
    })
    test_that("But if I unlock the dataset, I can edit the vars", {
        unlock(ds)
        expect_error(categories(ds$v4) <- rev(categories(ds$v4)),
            NA)
        expect_identical(names(categories(ds$v4)), c("No Data", "C", "B"))
    })
})
