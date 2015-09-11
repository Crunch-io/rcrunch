context("Deleting variables")

if (run.integration.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            test_that("deleteVariable(s)", {
                v1 <- ds$v1
                expect_true(all(c("v1", "v4") %in% names(ds)))
                ds <- deleteVariable(ds, c("v1", "v4"))
                expect_true(!any(c("v1", "v4") %in% names(ds)))
                expect_error(refresh(v1),
                    "Variable not found. It may have been deleted.")
            })
        })
        ## TODO: make deleteVariables fully delete arrays.
        
        with(test.dataset(df), {
            test_that("deleteVariables with consent", {
                with(temp.option(crunch.require.confirmation=TRUE), {
                    expect_true("v3" %in% names(ds))
                    expect_error(ds <- deleteVariable(ds, "v3"),
                        "Must confirm deleting variable")
                    with(consent(), {
                        ds <- deleteVariable(ds, "v3")
                        expect_true(is.null(ds$v3))
                        expect_true(is.null(refresh(ds)$v3))
                    })
                })
            })
        })
        
        with(test.dataset(df), {
            test_that("Delete variable by assigning NULL", {
                with(temp.option(crunch.require.confirmation=TRUE), {
                    expect_true("v3" %in% names(ds))
                    expect_identical(names(ds)[2], "v2")
                    expect_error(ds$v3 <- NULL,
                        "Must confirm deleting variable")
                    expect_error(ds[[2]] <- NULL,
                        "Must confirm deleting variable")
                    with(consent(), {
                        ds$v3 <- NULL
                        expect_true(is.null(ds$v3))
                        expect_true(is.null(refresh(ds)$v3))
                        ds[[2]] <- NULL
                        expect_true(is.null(ds$v2))
                        expect_true(is.null(refresh(ds)$v2))
                    })
                })
            })
        })
    })
}