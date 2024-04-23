context("Appending datasets with unbound subvariables")

with_test_authentication({
    whereas("When appending arrays with different subsets of subvariables", {
        part1 <- mrdf.setup(flakyRecoverNewDataset(mrdf[-3]), selections = "1.0")
        part1 <- saveVersion(part1, "Before appending")
        part2 <- mrdf.setup(flakyRecoverNewDataset(mrdf[-1]), selections = "1.0")
        test_that("set up MR for appending", {
            expect_true(is.Multiple(part1$MR))
            expect_identical(
                names(subvariables(part1$MR)),
                c("mr_1", "mr_2")
            )
            expect_equivalent(
                as.array(crtabs(~MR, data = part1)),
                array(c(2, 1),
                    dim = c(2L),
                    dimnames = list(MR = c("mr_1", "mr_2"))
                )
            )
            expect_true(is.Multiple(part2$MR))
            expect_identical(
                names(subvariables(part2$MR)),
                c("mr_2", "mr_3")
            )
            expect_equivalent(
                as.array(crtabs(~MR, data = part2)),
                array(c(1, 1),
                    dim = c(2L),
                    dimnames = list(MR = c("mr_2", "mr_3"))
                )
            )
        })
        out <- suppressMessages(try(appendDataset(part1, part2)))
        test_that("the arrays with different subvariables can append", {
            expect_true(is.dataset(out))
            expect_length(batches(out), 2)
            expect_identical(dim(out), c(nrow(mrdf) * 2L, 2L))
            expect_true(is.variable(out$MR))
            expect_true(is.Multiple(out$MR))
            expect_identical(
                names(subvariables(out$MR)),
                c("mr_1", "mr_2", "mr_3")
            )
            expect_equivalent(
                as.array(crtabs(~MR, data = out)),
                array(c(2, 2, 1),
                    dim = c(3L),
                    dimnames = list(MR = c("mr_1", "mr_2", "mr_3"))
                )
            )
        })

        test_that("Rolling back to initial import reverts the append", {
            out <- restoreVersion(out, "Before appending")
            expect_true(is.Multiple(out$MR))
            expect_identical(
                names(subvariables(out$MR)),
                c("mr_1", "mr_2")
            )
            expect_equivalent(
                as.array(crtabs(~MR, data = out)),
                array(c(2, 1),
                    dim = c(2L),
                    dimnames = list(MR = c("mr_1", "mr_2"))
                )
            )
            expect_length(batches(out), 2)
        })
    })
})
