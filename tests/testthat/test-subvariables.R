context("Subvariables")

with_mock_crunch({
    ds <- loadDataset("test ds")
    mr <- ds$mymrset

    test_that("setup", {
        expect_true(is.Multiple(mr))
    })

    test_that("subvariables are what we think", {
        expect_is(subvariables(mr), "Subvariables")
        expect_identical(names(subvariables(mr)), c("First", "Second", "Last"))
        expect_identical(
            aliases(subvariables(mr)),
            c("subvar2", "subvar1", "subvar3")
        )
    })

    test_that("subvariable name setter error checking", {
        expect_error(names(subvariables(mr)) <- 1:3)
        expect_error(names(subvariables(mr)) <- c("First", "Second"))
        expect_error(names(subvariables(mr)) <- c("First", "First", "First"))
    })
    test_that("subvariable name/alias setting", {
        expect_PATCH(
            names(subvariables(mr))[1:2] <- c("Uno", "Due"),
            "https://app.crunch.io/api/datasets/1/variables/mymrset/subvariables/", '{"https://app.crunch.io/api/datasets/1/variables/mymrset/subvariables/subvar2/":{"name":"Uno"},',
            '"https://app.crunch.io/api/datasets/1/variables/mymrset/subvariables/subvar1/":{"name":"Due"}}'
        )
        expect_PATCH(
            aliases(subvariables(mr))[1:2] <- c("uno", "due"),
            "https://app.crunch.io/api/datasets/1/variables/mymrset/subvariables/", '{"https://app.crunch.io/api/datasets/1/variables/mymrset/subvariables/subvar2/":{"alias":"uno"},',
            '"https://app.crunch.io/api/datasets/1/variables/mymrset/subvariables/subvar1/":{"alias":"due"}}'
        )
        expect_error(
            aliases(subvariables(mr)[c("Second", "NOTASUBVAR")]) <- c("uno", "due"),
            "Undefined subvariables selected: NOTASUBVAR"
        )
        expect_PATCH(
            names(subvariables(mr)[1:2]) <- c("Uno", "Due"),
            "https://app.crunch.io/api/datasets/1/variables/mymrset/subvariables/", '{"https://app.crunch.io/api/datasets/1/variables/mymrset/subvariables/subvar2/":{"name":"Uno"},',
            '"https://app.crunch.io/api/datasets/1/variables/mymrset/subvariables/subvar1/":{"name":"Due"}}'
        )
        expect_PATCH(
            names(subvariables(mr)[c("First", "Second")]) <- c("Uno", "Due"),
            "https://app.crunch.io/api/datasets/1/variables/mymrset/subvariables/", '{"https://app.crunch.io/api/datasets/1/variables/mymrset/subvariables/subvar2/":{"name":"Uno"},',
            '"https://app.crunch.io/api/datasets/1/variables/mymrset/subvariables/subvar1/":{"name":"Due"}}'
        )
    })

    test_that("[.Subvariables", {
        expect_is(subvariables(mr)[1:2], "Subvariables")
        expect_is(subvariables(mr)[c("First", "Last")], "Subvariables")
        expect_error(
            subvariables(mr)[c("First", "Other")],
            "Undefined subvariables selected"
        )
    })

    test_that("subvariable setter validation", {
        expect_error(
            subvariables(mr) <- Subvariables(),
            "Can only reorder, not change, subvariables"
        )
        expect_error(
            subvariables(mr) <- subvariables(mr)[1:2],
            "Can only reorder, not change, subvariables"
        )
        expect_PATCH(subvariables(mr) <- subvariables(mr)[c(3, 1, 2)])
        expect_error(
            subvariables(mr) <- 42,
            "Can only assign an object of class Subvariables"
        )
        expect_error(
            subvariables(mr) <- NULL,
            "Can only assign an object of class Subvariables"
        )
    })

    test_that("Assinging in with no changes does not make PATCH request", {
        expect_no_request(subvariables(mr) <- subvariables(mr))
    })

    test_that("can extract a subvariable as a Variable", {
        expect_is(subvariables(mr)[[1]], "CrunchVariable")
        expect_true(is.Categorical(subvariables(mr)[[1]]))
        expect_is(subvariables(mr)[["Second"]], "CrunchVariable")
        expect_true(is.Categorical(subvariables(mr)[["Second"]]))
        expect_is(subvariables(mr)$Second, "CrunchVariable")
        expect_true(is.Categorical(subvariables(mr)$Second))
        expect_null(subvariables(mr)$Other)
    })

    test_that("Validation when setting on a subvariable", {
        expect_error(
            name(subvariables(mr)[[4]]) <- "Four",
            "subscript out of bounds"
        )
        expect_error(
            subvariables(mr)[[2]] <- ds$gender,
            "Cannot add or remove subvariables"
        )
        expect_error(
            subvariables(mr)[[2]] <- NULL,
            "Cannot add or remove subvariables"
        )
        expect_error(
            subvariables(mr)[[2]] <- "not a variable",
            "Can only assign Variables into an object of class Subvariables"
        )
        expect_PATCH(
            name(subvariables(mr)$Second) <- "Due",
            "https://app.crunch.io/api/datasets/1/variables/mymrset/subvariables/",
            '{"https://app.crunch.io/api/datasets/1/variables/mymrset/subvariables/subvar1/":{"name":"Due"}}'
        )
    })

    test_that("Validation when setting on a [ subset of subvariables", {
        expect_error(
            names(subvariables(mr)[3:4]) <- c("3", "4"),
            "Subscript out of bounds: 4"
        )
        expect_error(
            subvariables(mr)[2:3] <- c("not a variable", "nor this"),
            "Can only assign Variables into an object of class Subvariables"
        )
    })

    test_that("can extract directly from array variable", {
        expect_true(is.Categorical(mr[[1]]))
        expect_true(is.Categorical(mr[["subvar1"]]))
        expect_true(is.Categorical(mr$subvar2))
        expect_null(mr$Other)

        expect_is(mr[c("subvar2", "subvar3")], "Subvariables")
        expect_identical(aliases(mr[c("subvar2", "subvar3")]), c("subvar2", "subvar3"))
        expect_identical(mr[, c("subvar2", "subvar3")], mr[c("subvar2", "subvar3")])
        expect_identical(mr[, c(1, 3)], mr[c("subvar2", "subvar3")])

        expect_error(
            mr[c("subvar2", "Other")],
            "Undefined subvariables selected: Other"
        )
        expect_error(
            mr[c("Different", "Other")],
            "Undefined subvariables selected: Different and Other"
        )
    })

    test_that("can extract directly from array variable with different namekey", {
        with(temp.option(crunch.namekey.array = "name"), {
            expect_true(is.Categorical(mr[[1]]))
            expect_true(is.Categorical(mr[["Second"]]))
            expect_true(is.Categorical(mr$Second))
            expect_null(mr$Other)

            expect_is(mr[c("First", "Last")], "Subvariables")
            expect_error(
                mr[c("First", "Other")],
                "Undefined subvariables selected: Other"
            )
        })
    })

    test_that("show method for Subvariables", {
        mr <- refresh(mr)
        expect_identical(showSubvariables(subvariables(mr)), c(
            "Subvariables:",
            "  $`First`",
            "  $`Second`",
            "  $`Last`"
        ))
    })
})

with_test_authentication({
    ds <- mrdf.setup(newDataset(mrdf), selections = "1.0")
    var <- ds$MR
    test_that("setup test case 2", {
        expect_true(is.Multiple(var))
        expect_identical(
            names(subvariables(var)),
            c("mr_1", "mr_2", "mr_3")
        )
        expect_equivalent(
            table(ds$MR),
            structure(array(c(2, 1, 1),
                dimnames = list(MR = c("mr_1", "mr_2", "mr_3"))
            ),
            class = "table"
            )
        )
    })

    test_that("can rename subvariables", {
        names(subvariables(var))[2] <- "M.R. Two"
        expect_identical(
            names(subvariables(var)),
            c("mr_1", "M.R. Two", "mr_3")
        )
    })

    test_that("can rename one subvariable", {
        name(subvariables(var)[[2]]) <- "Due"
        expect_identical(
            names(subvariables(var)),
            c("mr_1", "Due", "mr_3")
        )
        expect_identical(
            names(subvariables(refresh(var))),
            c("mr_1", "Due", "mr_3")
        )
        sv <- subvariables(var)
        name(sv[[2]]) <- "M.R. Two"
        expect_identical(
            names(sv),
            c("mr_1", "M.R. Two", "mr_3")
        )
    })
    test_that("can rename some subvariables", {
        names(subvariables(var)[2:3]) <- c("Dois", "Tres")
        expect_identical(
            names(subvariables(var)),
            c("mr_1", "Dois", "Tres")
        )
        sv <- subvariables(var)
        names(sv[2:3]) <- c("M.R. Two", "mr_3")
        expect_identical(
            names(sv),
            c("mr_1", "M.R. Two", "mr_3")
        )
    })
    test_that("subvariables aliases", {
        expect_identical(
            aliases(subvariables(var)),
            c("mr_1", "mr_2", "mr_3")
        )
        aliases(subvariables(var)) <- paste0("mr_", 5:7)
        expect_identical(
            aliases(subvariables(var)),
            c("mr_5", "mr_6", "mr_7")
        )
        expect_identical(
            aliases(subvariables(refresh(var))),
            c("mr_5", "mr_6", "mr_7")
        )
    })

    subvariables(ds$MR) <- subvariables(ds$MR)[c(3, 1, 2)]
    test_that("Can reorder subvariables", {
        expect_identical(
            names(subvariables(ds$MR)),
            c("mr_3", "mr_1", "M.R. Two")
        )
        expect_equivalent(
            table(ds$MR),
            structure(array(c(1, 2, 1),
                dimnames = list(MR = c("mr_3", "mr_1", "M.R. Two"))
            ),
            class = "table"
            )
        )
    })

    ## Refresh the dataset and confirm the metadata change
    ds <- refresh(ds)
    test_that("Reordering of subvars persists on refresh", {
        expect_identical(
            names(subvariables(ds$MR)),
            c("mr_3", "mr_1", "M.R. Two")
        )
        expect_equivalent(
            table(ds$MR),
            structure(array(c(1, 2, 1),
                dimnames = list(MR = c("mr_3", "mr_1", "M.R. Two"))
            ),
            class = "table"
            )
        )
    })

    ## Check that that persisted on release/reload
    ds <- releaseAndReload(ds)
    test_that("Reordering of subvars persists on release", {
        expect_identical(
            names(subvariables(ds$MR)),
            c("mr_3", "mr_1", "M.R. Two")
        )
        expect_equivalent(
            table(ds$MR),
            structure(array(c(1, 2, 1),
                dimnames = list(MR = c("mr_3", "mr_1", "M.R. Two"))
            ),
            class = "table"
            )
        )
    })

    subvariables(ds$MR)[1:2] <- subvariables(ds$MR)[c(2, 1)]
    test_that("Can reorder a subset of subvariables", {
        expect_identical(
            names(subvariables(ds$MR)),
            c("mr_1", "mr_3", "M.R. Two")
        )
    })

    name(ds$MR$mr_5) <- "MR Five"
    test_that("Can edit name of subvariable with variable setter", {
        expect_identical(
            names(subvariables(ds$MR)),
            c("MR Five", "mr_3", "M.R. Two")
        )
    })

    test_that("Can edit alias of subvariable with variable setter", {
        expect_identical(
            aliases(subvariables(ds$MR)),
            c("mr_5", "mr_7", "mr_6")
        )
        alias(ds$MR$mr_5) <- "mr_five"
        expect_identical(
            aliases(subvariables(ds$MR)),
            c("mr_five", "mr_7", "mr_6")
        )
    })

    ds <- mrdf.setup(restoreVersion(ds, "initial import"), selections = "1.0")
    ds$MRcopy <- copy(ds$MR)

    test_that("Initial subvariable orders and counts", {
        expect_identical(
            names(subvariables(ds$MR)),
            c("mr_1", "mr_2", "mr_3")
        )
        expect_equivalent(
            as.array(crtabs(~MR, data = ds)),
            structure(array(c(2, 1, 1),
                dimnames = list(MR = c("mr_1", "mr_2", "mr_3"))
            ))
        )
        expect_equivalent(
            as.array(crtabs(~MRcopy, data = ds)),
            structure(array(c(2, 1, 1),
                dimnames = list(MR = c("mr_1", "mr_2", "mr_3"))
            ))
        )
    })

    subvariables(ds$MRcopy) <- subvariables(ds$MRcopy)[c(2, 3, 1)]
    test_that("Can reorder the copy", {
        expect_equivalent(
            as.array(crtabs(~MR, data = ds)),
            structure(array(c(2, 1, 1),
                dimnames = list(MR = c("mr_1", "mr_2", "mr_3"))
            ))
        )
        expect_equivalent(
            as.array(crtabs(~MRcopy, data = ds)),
            structure(array(c(1, 1, 2),
                dimnames = list(MR = c("mr_2", "mr_3", "mr_1"))
            ))
        )
    })

    ds <- releaseAndReload(ds)
    test_that("Still reordered on release", {
        expect_equivalent(
            as.array(crtabs(~MR, data = ds)),
            structure(array(c(2, 1, 1),
                dimnames = list(MR = c("mr_1", "mr_2", "mr_3"))
            ))
        )
        expect_equivalent(
            as.array(crtabs(~MRcopy, data = ds)),
            structure(array(c(1, 1, 2),
                dimnames = list(MR = c("mr_2", "mr_3", "mr_1"))
            ))
        )
    })

    test_that("Can append after reordering", {
        with(test.dataset(mrdf, "part2"), {
            part2 <- mrdf.setup(part2, selections = "1.0")
            ds <- appendDataset(ds, part2)
            expect_equivalent(
                as.array(crtabs(~MR, data = ds)),
                structure(array(c(4, 2, 2),
                    dimnames = list(MR = c("mr_1", "mr_2", "mr_3"))
                ))
            )
            expect_equivalent(
                as.array(crtabs(~MRcopy, data = ds)),
                structure(array(c(2, 2, 4),
                    dimnames = list(MR = c("mr_2", "mr_3", "mr_1"))
                ))
            )
        })
    })

    test_that("Can copy again after appending", {
        ds$MRcopy2 <- copy(ds$MR, name = "Mister copy two")
        ds$MRcopy3 <- copy(ds$MRcopy, name = "Mister copy three")
        ds$v4copy <- copy(ds$v4)
        expect_equivalent(
            as.array(crtabs(~MRcopy2, data = ds)),
            structure(array(c(4, 2, 2),
                dimnames = list(MR = c("mr_1", "mr_2", "mr_3"))
            ))
        )
        expect_equivalent(
            as.array(crtabs(~MRcopy3, data = ds)),
            structure(array(c(2, 2, 4),
                dimnames = list(MR = c("mr_2", "mr_3", "mr_1"))
            ))
        )
        expect_identical(as.vector(ds$v4copy), as.vector(ds$v4))
    })

    with(test.dataset(mrdf["mr_1"]), {
        ds <- mrdf.setup(ds)

        test_that("Setup for tests with array with one subvar", {
            expect_length(subvariables(ds$CA), 1)
            expect_identical(names(subvariables(ds$CA)), "mr_1")
            expect_identical(
                names(categories(ds$CA)),
                c("0.0", "1.0", "No Data")
            )
        })

        test_that("Can edit category names", {
            names(categories(ds$CA))[1:2] <- c("False", "True")
            expect_identical(
                names(categories(ds$CA)),
                c("False", "True", "No Data")
            )
        })

        test_that("Can edit name of single-subvar", {
            names(subvariables(ds$CA)) <- "MR_1"
            expect_identical(names(subvariables(ds$CA)), "MR_1")
        })
    })
})
