context("Fork and merge")

if (run.integration.tests) {
    with_test_authentication({
        with(test.dataset(df), {
            test_that("Fork catalog exists", {
                expect_is(forks(ds), "ForkCatalog")
                expect_length(forks(ds), 0)
            })

            f1 <- forkDataset(ds)
            f1.name <- paste("Fork of", name(ds))
            test_that("Can create a new fork", {
                expect_true(is.dataset(f1))
                expect_identical(name(f1), f1.name)
                expect_identical(names(forks(ds)), f1.name)
                expect_true(is.published(f1))
            })

            exclusion(f1) <- f1$v3 < 11
            f2 <- forkDataset(f1, "Fork yeah!", draft=TRUE)
            test_that("Can create a fork with a given name (forking from a fork)", {
                expect_true(is.dataset(f2))
                expect_identical(name(f2), "Fork yeah!")
                expect_true("Fork yeah!" %in% names(forks(f1)))
                expect_false(is.published(f2))
            })

            test_that("Forking preserves exclusion filters", {
                expect_output(exclusion(f1), "v3 < 11")
                expect_output(exclusion(f2), "v3 < 11")
                expect_identical(nrow(f2), 17L)
                expect_identical(dim(f1), dim(f2))
            })

            f3 <- forkDataset(ds, draft=FALSE)
            f3.name <- paste("Fork #2 of", name(ds))
            test_that("Creating forks autonames with a fork number", {
                expect_true(is.dataset(f3))
                expect_identical(name(f3), f3.name)
                expect_true(setequal(names(forks(ds)),
                    c(f1.name, f3.name)))
                expect_true(is.published(f3))
            })

            delete(f2)
            delete(f3)
            test_that("If you delete a fork, it disappears from upstream forks catalog", {
                expect_identical(names(refresh(forks(ds))), f1.name)
            })

            ## Make edits to fork #1. cf. test-versioning.R:
            # 0. There's an exclusion, set above.
            # 1. Edit variable metadata
            names(categories(f1$v4))[1:2] <- c("d", "e")
            name(f1$v2) <- "Variable Two"
            description(f1$v3) <- "The third variable in the dataset"

            # 2. Edit dataset metadata
            description(f1) <- "A dataset for testing"

            # 3. Reorder variables
            ordering(f1) <- VariableOrder(VariableGroup("Even", f1[c(2,4,6)]),
                VariableGroup("Odd", f1[c(1,3,5)]))

            # 5. Add non-derived variable
            f1$v8 <- rep(1:5, 4)[4:20]

            # 4. Derive variable
            f1$v7 <- f1$v3 - 6

            ## Assert those things
            test_that("The edits are made to the fork", {
                expect_output(exclusion(f1), "v3 < 11")
                expect_identical(dim(f1), c(17L, 8L))
                expect_identical(names(na.omit(categories(f1$v4))),
                    c("d", "e"))
                expect_identical(name(f1$v2), "Variable Two")
                expect_identical(description(f1$v3),
                    "The third variable in the dataset")
                expect_identical(description(f1), "A dataset for testing")
                expect_identical(as.vector(f1$v7), df$v3[4:20] - 6)
                expect_equivalent(as.vector(f1$v8), rep(1:5, 4)[4:20])
                expect_identical(aliases(variables(f1)),
                    paste0("v", c(2,4,6,1,3,5,8,7)))
            })

            test_that("The upstream dataset is unaffected by edits to the fork", {
                validImport(ds)
            })

            ## So that f1 gets cleaned up even if merge fails
            with(test.dataset(f1, "f1"), {
                ## Now merge f1 back to ds
                with_silent_progress({
                    ## Don't print the progress bar so our test output is clean
                    ds <- mergeFork(ds, f1)
                })
                test_that("The edits made to the fork are now upstream", {
                    expect_output(exclusion(ds), "v3 < 11")
                    expect_identical(dim(ds), c(17L, 8L))
                    expect_identical(names(na.omit(categories(ds$v4))),
                        c("d", "e"))
                    expect_identical(name(ds$v2), "Variable Two")
                    expect_identical(description(ds$v3),
                        "The third variable in the dataset")
                    expect_identical(as.vector(ds$v7), df$v3[4:20] - 6)
                    expect_equivalent(as.vector(ds$v8), rep(1:5, 4)[4:20])
                    expect_identical(aliases(variables(ds)),
                        paste0("v", c(2,4,6,1,3,5,8,7)))
                    ## Extra checks for v7 and v8
                    expect_true("v7" %in% aliases(allVariables(ds)))
                    expect_true("v8" %in% aliases(allVariables(ds)))
                })
                test_that("Certain changes don't merge", {
                    expect_identical(description(ds), "")
                })
            })
        })
    })
}
