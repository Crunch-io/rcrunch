context("Clone and graft datasets")

skip({
if (run.integration.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            clone <- try(cloneDataset(ds))
            test_that("Cloning happens", {
                expect_false(identical(self(ds), self(clone)))
                expect_identical(names(ds), names(clone))
            })
            
            try(alias(clone$v1) <- "v1_NEW")
            test_that("Grafting setup", {
                expect_false(identical(names(ds), names(clone)))
            })
            try(ds <- graftDataset(ds, clone))
            test_that("Grafting works", {
                expect_identical(names(ds), names(clone))
            })
        })
    })
}
})