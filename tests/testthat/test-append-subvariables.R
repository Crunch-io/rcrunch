context("Appending datasets with unbound subvariables")

with_test_authentication({
    whereas("When appending a dataset with unbound subvariables", {
        part1 <- mrdf.setup(newDataset(mrdf), selections = "1.0")
        mr_cats <- categories(part1$MR)
        subvar_cats <- categories(part1$MR$mr_1)
        dichotomized_cats <- Categories(
            list(id = 2L, missing = FALSE, name = "0.0", numeric_value = 0, selected = FALSE),
            list(id = 1L, missing = FALSE, name = "1.0", numeric_value = 1, selected = TRUE),
            list(id = -1L, missing = TRUE, name = "No Data", numeric_value = NULL, selected = FALSE)
        )
        ## Dichotomize this way so that categories get aligned
        ## (via supertype)

        part2 <- mrdf.setup(newDataset(mrdf))
        unbind(part2$CA)
        part2 <- refresh(part2)
        undichotomized_cats <- Categories(
            list(id = 2L, missing = FALSE, name = "0.0", numeric_value = 0),
            list(id = 1L, missing = FALSE, name = "1.0", numeric_value = 1),
            list(id = -1L, missing = TRUE, name = "No Data", numeric_value = NULL)
        )
        test_that("set up MR for appending", {
            expect_true(is.Multiple(part1$MR))
            expect_equivalent(
                as.array(crtabs(~MR, data = part1)),
                array(c(2, 1, 1),
                    dim = c(3L),
                    dimnames = list(MR = c("mr_1", "mr_2", "mr_3"))
                )
            )
            expect_null(part2$MR)
            expect_identical(mr_cats, subvar_cats)
            expect_identical(mr_cats, dichotomized_cats)
            expect_identical(
                categories(part2$mr_1),
                undichotomized_cats
            )
            expect_false(identical(
                dichotomized_cats,
                undichotomized_cats
            )) ## Just being clear about that
            expect_identical(
                as.vector(part1$MR$mr_1),
                as.vector(part2$mr_1)
            )
            expect_identical(
                as.vector(part1$MR$mr_2),
                as.vector(part2$mr_2)
            )
            expect_identical(
                as.vector(part1$MR$mr_3),
                as.vector(part2$mr_3)
            )
        })
        out <- suppressMessages(try(appendDataset(part1, part2)))
        test_that("Dataset #2 isn't modified by appending to another", {
            part2 <- refresh(part2)
            expect_null(part2$MR)
            expect_true(is.Categorical(part2$mr_1))
        })
        test_that("the unbound subvariables get lined up", {
            expect_true(is.dataset(out))
            expect_length(batches(out), 2)
            expect_identical(dim(out), c(nrow(mrdf) * 2L, 2L))
            expect_true(is.variable(out$MR))
            expect_identical(categories(out$MR), dichotomized_cats)
            expect_identical(categories(out$MR$mr_1), dichotomized_cats)
            expect_false(identical(
                categories(out$MR),
                undichotomized_cats
            ))
            expect_identical(
                as.vector(out$MR$mr_1),
                rep(as.vector(part2$mr_1), 2)
            )
            expect_true(is.Multiple(out$MR))
            expect_identical(
                names(subvariables(out$MR)),
                c("mr_1", "mr_2", "mr_3")
            )
            expect_equivalent(
                as.array(crtabs(~MR, data = out)),
                array(c(4, 2, 2),
                    dim = c(3L),
                    dimnames = list(MR = c("mr_1", "mr_2", "mr_3"))
                )
            )
        })
    })

    whereas("When appending arrays with different subsets of subvariables", {
        part1 <- mrdf.setup(newDataset(mrdf[-3]), selections = "1.0")
        part1 <- saveVersion(part1, "Before appending")
        part2 <- mrdf.setup(newDataset(mrdf[-1]), selections = "1.0")
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
