context("Appending datasets with arrays")

with_test_authentication({
    whereas("Appending arrays with mismatching names and aliases", {
        part1 <- mrdf.setup(newDataset(mrdf), selections = "1.0")
        names(subvariables(part1$MR)) <- c("One", "Two", "Three")
        ## Aliases are c("mr_1", "mr_2", "mr_3")
        part2 <- mrdf.setup(newDataset(mrdf), selections = "1.0")
        names(subvariables(part2$MR)) <- c("Loneliest", "Two", "Three")
        aliases(subvariables(part2$MR))[3] <- "alt"
        ## Aliases are c("mr_1", "mr_2", "alt")

        test_that("compareDatasets sees the mismatch", {
            expect_prints(
                summary(compareDatasets(part1, part2)),
                paste(
                    "Mismatched names: 2 ",
                    " name.A alias name.B",
                    "  Three  mr_3   <NA>",
                    "   <NA>   alt  Three",
                    sep = "\n"
                )
            )
        })

        out <- appendDataset(part1, part2)

        test_that("We can append despite the duplicate name", {
            expect_true(is.dataset(out))
            expect_length(batches(out), 3)
            expect_identical(dim(out), c(nrow(mrdf) * 2L, 2L))
            expect_true(is.Multiple(out$MR))
            skip("We get 2 'Threes'")
            expect_equivalent(
                as.array(crtabs(~MR, data = out)),
                array(c(4, 2, 2),
                    dim = c(3L),
                    dimnames = list(MR = c("One", "Two", "Three"))
                )
            )
        })
    })

    whereas("When appending and reverting and reloading", {
        part1 <- newDatasetFromFixture("apidocs")
        test_that("Setup for testing references post append", {
            expect_true(name(part1$allpets) == "All pets owned")
            name(part1$allpets) <- "Some of my pets"
            expect_true(name(part1$allpets) == "Some of my pets")
        })

        ## Release and re-lease
        part1 <- releaseAndReload(part1)

        test_that("Check again", {
            expect_true(name(part1$allpets) == "Some of my pets")
        })

        part2 <- newDatasetFromFixture("apidocs")
        out <- suppressMessages(try(appendDataset(part1, part2)))
        test_that("Append doesn't revert metadata changes", {
            expect_false(name(out$allpets) == "All pets owned")
            expect_true(name(out$allpets) == "Some of my pets")
        })

        ## Release and re-lease
        out <- releaseAndReload(out)

        test_that("Metadata sticks after releasing", {
            expect_false(name(out$allpets) == "All pets owned")
            expect_true(name(out$allpets) == "Some of my pets")
        })

        ## Change the name and release again
        name(out$allpets) <- "Apple"
        out <- releaseAndReload(out)
        test_that("Metadata sticks after releasing and not appending", {
            expect_true(name(out$allpets) == "Apple")
        })
    })
})
