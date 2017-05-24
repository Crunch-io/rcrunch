context("Appending datasets with arrays")

with_test_authentication({
    whereas("when appending identical datasets with arrays", {
        part1 <- mrdf.setup(newDataset(mrdf), selections="1.0")
        part2 <- mrdf.setup(newDataset(mrdf), selections="1.0")
        test_that("they get set up correctly", {
            expect_true(is.Multiple(part1$MR))
            expect_equivalent(as.array(crtabs(~ MR, data=part1)),
                array(c(2, 1, 1), dim=c(3L),
                dimnames=list(MR=c("mr_1", "mr_2", "mr_3"))))
            expect_true(is.Multiple(part2$MR))
            expect_equivalent(as.array(crtabs(~ MR, data=part2)),
                array(c(2, 1, 1), dim=c(3L),
                dimnames=list(MR=c("mr_1", "mr_2", "mr_3"))))
            # 2 because there is always a "batch 0" which
            # is present for rows which were added directly
            # rather than through a batch append,
            # and another for "part1".
            expect_length(batches(part1), 2)
            expect_length(batches(part2), 2)
        })
        test_that("compareDatasets says they're all good", {
            expect_output(summary(compareDatasets(part1, part2)),
                "All good :)")
        })
        test_that("they append successfully", {
            out <- appendDataset(part1, part2, autorollback=FALSE)
            expect_true(is.dataset(out))
            expect_length(batches(out), 3)
            expect_identical(dim(out), c(nrow(mrdf)*2L, 2L))
            expect_true(is.Multiple(out$MR))
            expect_equivalent(as.array(crtabs(~ MR, data=out)),
                array(c(4, 2, 2), dim=c(3L),
                dimnames=list(MR=c("mr_1", "mr_2", "mr_3"))))
            expect_true(is.dataset(refresh(part2)))
        })
    })


    whereas("When appending arrays with sparse data", {
        part1 <- newDataset(data.frame(v4=factor(rep(c("a", "b"), 500))))
        part2 <- mrdf.setup(newDataset(data.frame(
                            mr_1=c(1,0,1,1,0, rep(NA, 995)),
                            mr_2=c(rep(NA, 995), 0, 1, 1, 1, 0),
                            v4=as.factor(LETTERS[2:3]))))
        test_that("compareDatasets identifies the category id mismatch", {
            expect_output(summary(compareDatasets(part1, part2)),
                paste(
                    "Mismatched ids: 4 ",
                    " id.A name id.B",
                    "    1    a   NA",
                    "    2    b   NA",
                    "   NA    B    1",
                    "   NA    C    2",
                    sep="\n"))
        })
        out <- suppressMessages(try(appendDataset(part1, part2)))
        test_that("the sparse arrays append", {
            expect_length(batches(out), 3)
            expect_identical(nrow(out), 2000L)
            expect_identical(as.vector(out$CA$mr_2),
                factor(c(rep(NA, 1995), "0.0", "1.0", "1.0", "1.0", "0.0")))
        })

        test_that("Rolling back to initial import reverts the append", {
            out <- restoreVersion(out, length(versions(out))) ## Get the oldest
            expect_identical(nrow(out), 1000L)
            expect_length(batches(out), 2)
        })
    })

    whereas("Appending arrays with mismatching names and aliases", {
        part1 <- mrdf.setup(newDataset(mrdf), selections="1.0")
        names(subvariables(part1$MR)) <- c("One", "Two", "Three")
        ## Aliases are c("mr_1", "mr_2", "mr_3")
        part2 <- mrdf.setup(newDataset(mrdf), selections="1.0")
        names(subvariables(part2$MR)) <- c("Loneliest", "Two", "Three")
        aliases(subvariables(part2$MR))[3] <- "alt"
        ## Aliases are c("mr_1", "mr_2", "alt")

        test_that("compareDatasets sees the mismatch", {
            expect_output(summary(compareDatasets(part1, part2)),
                paste(
                    "Mismatched names: 2 ",
                    " name.A alias name.B",
                    "  Three  mr_3   <NA>",
                    "   <NA>   alt  Three",
                    sep="\n"))
        })

        out <- appendDataset(part1, part2)

        test_that("We can append despite the duplicate name", {
            expect_true(is.dataset(out))
            expect_length(batches(out), 3)
            expect_identical(dim(out), c(nrow(mrdf)*2L, 2L))
            expect_true(is.Multiple(out$MR))
            skip("We get 2 'Threes'")
            expect_equivalent(as.array(crtabs(~ MR, data=out)),
                array(c(4, 2, 2), dim=c(3L),
                dimnames=list(MR=c("One", "Two", "Three"))))
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
