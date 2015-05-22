context("Weights")

if (run.integration.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            test_that("Can set weight variable", {
                expect_identical(weight(ds), NULL)
                weight(ds) <- ds$v3
                expect_equivalent(weight(ds), ds$v3)
                ds <- refresh(ds)
                expect_equivalent(weight(ds), ds$v3)
                weight(ds) <- NULL
                expect_identical(weight(ds), NULL)
            })
            test_that("Errors are properly handled when setting weight", {
                expect_error(weight(ds) <- "a", 
                    "Weight must be a Variable or NULL")
                ## test error handling when trying to set non-numeric
            })
            
            test_that("If weight is set, computations are weighted", {
                expect_equivalent(table(ds$v4), 
                    structure(c(B=10, C=10), class="table"))
                weight(ds) <- ds$v3
                expect_equivalent(table(ds$v4), 
                    structure(c(B=sum(seq(8, 26, 2)), C=sum(seq(9, 27, 2))), 
                    class="table"))
            })
            
            test_that("If weight is set, dim() is still unweighted", {
                weight(ds) <- NULL
                expect_identical(nrow(ds), 20L)
                weight(ds) <- ds$v3
                expect_identical(nrow(ds), 20L)
                ds <- refresh(ds)
                expect_identical(nrow(ds), 20L)
            })
        })
    })
}