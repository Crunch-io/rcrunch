context("Deleting rows of a dataset")

with(fake.HTTP, {
    ds <- loadDataset("test ds")
    test_that("dropRows generates the right request", {
        expect_error(dropRows(ds, ds$gender == "Male"),
            "INSERT ERROR MESSAGE HERE")
    })
})

if (run.integration.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            test_that("dropRows really removes rows", {
                try(ds <- dropRows(ds, ds$v4 == "C"))
                expect_identical(dim(ds), c(10L, ncol(df)))
            })
        })
    })
}
