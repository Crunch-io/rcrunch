context("Exclusion filters")

with_mock_crunch({
    ds <- cachedLoadDataset("test ds")
    ds2 <- cachedLoadDataset("ECON.sav")

    test_that("Get exclusions", {
        expect_equivalent(exclusion(ds), ds$birthyr < 0)
        expect_null(exclusion(ds2))
    })
    test_that("Set exclusion", {
        expect_PATCH(
            exclusion(ds) <- NULL,
            "https://app.crunch.io/api/datasets/1/exclusion/",
            '{"expression":{}}'
        )
        expect_PATCH(
            exclusion(ds) <- ds$birthyr == 1981,
            "https://app.crunch.io/api/datasets/1/exclusion/",
            '{"expression":{"function":"==","args":',
            '[{"variable":"https://app.crunch.io/api/datasets/1/variables/birthyr/"},',
            '{"value":1981}]}}'
        )
    })

    test_that("Validation for setting exclusion", {
        expect_error(
            exclusion(ds) <- "Not a filter",
            paste(
                dQuote("value"),
                "must be a CrunchLogicalExpr or NULL, not",
                dQuote("character")
            )
        )
    })
})

with_test_authentication({
    with(test.dataset(df), {
        test_that("We can set and unset an exclusion filter", {
            ds <- refresh(ds)
            expect_null(activeFilter(ds))
            expect_identical(nrow(ds), 20L)
            expect_equivalent(
                as.array(crtabs(~v4, data = ds)),
                array(c(10, 10), dim = 2L, dimnames = list(v4 = c("B", "C")))
            )
            expect_null(exclusion(ds))

            exclusion(ds) <- ds$v4 == "C"
            ## Test that the filter is set correctly. Objects not identical
            ## because JSON objects are unordered.
            expect_json_equivalent(zcl(exclusion(ds)), zcl(ds$v4 == "C"))
            expect_prints(
                exclusion(ds),
                'Crunch logical expression: v4 == "C"'
            )

            expect_identical(nrow(ds), 10L)
            expect_equivalent(
                as.array(crtabs(~v4, data = ds)),
                array(c(10, 0), dim = 2L, dimnames = list(v4 = c("B", "C")))
            )

            exclusion(ds) <- NULL
            expect_null(exclusion(ds))
            expect_identical(nrow(ds), 20L)
            expect_equivalent(
                as.array(crtabs(~v4, data = ds)),
                array(c(10, 10), dim = 2L, dimnames = list(v4 = c("B", "C")))
            )
        })
        ds <- refresh(ds)

        test_that("Exclusion setup", {
            expect_identical(nrow(ds), 20L)
            exclusion(ds) <<- ds$v4 == "C"
            expect_identical(nrow(ds), 10L)
        })

        test_that("Add a variable", {
            ds$newvar1 <- 1:10
            expect_equivalent(as.vector(ds$newvar1), 1:10)
            exclusion(ds) <- NULL
            expect_equivalent(
                as.vector(ds$newvar1),
                c(
                    1, NA, 2, NA, 3, NA, 4, NA, 5, NA, 6, NA, 7, NA, 8, NA,
                    9, NA, 10, NA
                )
            )
        })

        test_that("Change a variable's type", {
            exclusion(ds) <- ds$v4 == "C"
            expect_identical(nrow(ds), 10L)
            type(ds$v3) <- "text"
            expect_equivalent(
                as.vector(ds$v3),
                c("8.0", "10.0", "12.0", "14.0", "16.0", "18.0", "20.0", "22.0", "24.0", "26.0")
            )
        })

        test_that("Remove an exclusion and then update the variable we added", {
            ds <- refresh(ds)
            exclusion(ds) <- NULL
            expect_identical(nrow(ds), 20L)
            ds$newvar1[is.na(ds$newvar1)] <- 0
            expect_equivalent(
                as.vector(ds$newvar1),
                c(
                    1, 0, 2, 0, 3, 0, 4, 0, 5, 0, 6, 0, 7, 0, 8, 0,
                    9, 0, 10, 0
                )
            )
        })
    })
    with(test.dataset(df), {
        test_that("Update a variable", {
            exclusion(ds) <- ds$v4 == "C"
            expect_equivalent(nrow(ds), 10L)
            ds$v3 <- 10:1
            expect_equivalent(as.vector(ds$v3), 10:1)
            exclusion(ds) <- NULL
            expect_equivalent(
                as.vector(ds$v3),
                c(
                    10, 9, 9, 11, 8, 13, 7, 15, 6, 17, 5, 19, 4, 21, 3, 23,
                    2, 25, 1, 27
                )
            )
        })

        test_that("Update a variable by row index", {
            ## Setup: add a new numeric variable to mess with
            exclusion(ds) <<- NULL
            ds$v3x <- 4
            expect_equivalent(as.vector(ds$v3x), rep(4, 20))

            exclusion(ds) <<- ds$v4 == "C"
            ds$v3x[2] <- 9
            expect_equivalent(as.vector(ds$v3x), c(4, 9, rep(4, 8)))
            exclusion(ds) <<- NULL
            expect_equivalent(as.vector(ds$v3x), c(rep(4, 2), 9, rep(4, 17)))
        })

        test_that("Append to a dataset", {
            with(test.dataset(df, "part1"), {
                exclusion(ds) <<- ds$v4 == "C"
                out <- appendDataset(part1, ds)
                expect_identical(nrow(out), 30L)
                expect_equivalent(
                    as.vector(out$v4),
                    as.factor(c(rep(LETTERS[2:3], 10), rep("B", 10)))
                )
            })
        })
    })

    test_that("Append another dataset to one with exclusion", {
        with(test.dataset(df, "part1"), {
            with(test.dataset(df, "part2"), {
                exclusion(part1) <- part1$v4 == "C"
                out <- appendDataset(part1, part2)
                expect_identical(nrow(out), 20L)
                expect_equivalent(
                    as.array(crtabs(~v4,
                        data = out
                    )),
                    array(c(20, 0), dim = 2L, dimnames = list(v4 = c("B", "C")))
                )
            })
        })
    })

    with(test.dataset(newDatasetFromFixture("apidocs")), {
        ## Create a variable to update with rows to exclude
        ds$keep <- TRUE
        test_that("'keep' was created correctly, i.e. all 'True'", {
            expect_identical(
                as.array(crtabs(~keep, data = ds)),
                array(c(20, 0),
                    dim = 2L,
                    dimnames = list(keep = c("True", "False"))
                )
            )
        })
        ## Set the exclusion filter
        exclusion(ds) <- ds$keep == "False"
        test_that("We still have nrow=20, i.e. all rows", {
            expect_identical(nrow(ds), 20L)
        })


        ## check with new cube behavior
        ## Let's look at "allpets", a multiple response variable
        test_that("allpets", {
            expect_identical(
                as.array(crtabs(~allpets,
                    data = ds,
                    useNA = "ifany"
                )),
                array(c(4, 5, 5),
                    dim = 3L,
                    dimnames = list(allpets = c("Cat", "Dog", "Bird"))
                )
            )
        })

        ## Update "keep" as "False" (i.e. excluded) where "allpets" is
        ## missing for all responses.
        ds$keep[is.na(ds$allpets)] <- "False"
        test_that("The exclusion filter drops those three rows", {
            expect_identical(nrow(ds), 17L)
            ## And we can confirm that those three rows are the ones
            ## dropped:
            expect_identical(
                as.array(crtabs(~allpets,
                    data = ds,
                    useNA = "always"
                )),
                array(c(4, 5, 5),
                    dim = 3L,
                    dimnames = list(allpets = c("Cat", "Dog", "Bird"))
                )
            )
        })

        ## We can do the same for categorical array. petloc has two
        ## subvariables, Home and Work:
        test_that("Home x Work", {
            expect_identical(
                as.array(crtabs(~ petloc$petloc_home + petloc$petloc_work,
                    data = ds, useNA = "ifany"
                )),
                array(c(
                    1, 0, 1, 3, 0,
                    0, 0, 2, 0, 1,
                    1, 2, 0, 0, 3,
                    1, 1, 0, 0, 0,
                    0, 0, 0, 1, 0
                ),
                dim = c(5L, 5L),
                dimnames = list(
                    petloc_home = c(
                        "Cat", "Dog", "Bird", "Skipped",
                        "Not Asked"
                    ),
                    petloc_work = c(
                        "Cat", "Dog", "Bird", "Skipped",
                        "Not Asked"
                    )
                )
                )
            )
        })
        ## Notice there is one value that is missing for both (Skipped on
        ## one, Not Asked on the other).
        test_that("is.na on array is TRUE where all subvars are missing", {
            expect_equivalent(as.array(crtabs(~ is.na(petloc), data = ds))["TRUE"], 1)
        })
        ## Update "keep" with that expression
        ds$keep[is.na(ds$petloc)] <- "False"
        test_that("The exclusion filter now drops that row too", {
            expect_identical(nrow(ds), 16L)
        })

        ## We can add more complex expressions too. Let's assume that we
        ## know that it is not possible for anyone in Austria to have
        ## a dog. Yet in our data, there are two people who live in
        ## Austria yet have a dog (one has 2, the other has 3: this is the
        ## third "row" in the array below (keeping in mind that R stores
        ## arrays as stacked columns, so the third "row" below is the
        ## column of the table that corresponds to Austria))
        test_that("ndogs by country", {
            expect_identical(
                as.array(crtabs(~ ndogs + country,
                    data = ds, useNA = "no"
                )),
                array(c(
                    0, 1, 2, 0, 0,
                    0, 0, 1, 0, 0,
                    1, 0, 1, 1, 0,
                    0, 0, 1, 1, 0,
                    0, 0, 2, 0, 1
                ),
                dim = c(5L, 5L),
                dimnames = list(
                    ndogs = c(0, 1, 2, 3, 6),
                    country = c(
                        "Argentina", "Australia", "Austria",
                        "Belgium", "Brazil"
                    )
                )
                )
            )
            ## Or, perhaps more clear to view the frequencies of "ndogs" on
            ## the dataset filtered to just "Argentina"
            expect_identical(
                as.array(crtabs(~ndogs,
                    data = ds[ds$country == "Austria", ]
                )),
                array(c(1, 1, 1),
                    dim = 3L,
                    dimnames = list(ndogs = c(0, 2, 3))
                )
            )
        })
        ## So let's also drop rows corresponding to Austrians with >0 dogs:
        ds$keep[ds$country == "Austria" & ds$ndogs > 0] <- "False"
        test_that("The excluded nrow goes down by 2", {
            expect_identical(nrow(ds), 14L)
        })

        test_that("If I delete a variable on which the exclusion depends, we're ok", {
            with(consent(), ds$keep <- NULL)
            expect_null(ds$keep)
            expect_null(exclusion(ds))
            expect_identical(nrow(ds), 20L)
        })
        test_that("Exclude on is.na, then delete variable", {
            exclusion(ds) <- is.na(ds$q1) | is.na(ds$q3)
            expect_identical(nrow(ds), 10L)
            with(consent(), ds$q1 <- NULL)
            expect_null(ds$q1)
            expect_null(exclusion(ds))
            expect_identical(nrow(ds), 20L)
        })


        test_that("Exclusion is set", {
            ds <- refresh(ds)
            ds$keep <- rep(1:4, 5)
            exclusion(ds) <- ds$keep == 2

            expect_identical(nrow(ds), 15L)
        })

        test_that("No problem reverting to before exclusion var made", {
            ds <- restoreVersion(ds, 1)

            expect_valid_apidocs_import(ds)
            expect_null(ds$keep)
            expect_null(exclusion(ds))
        })
    })
})
