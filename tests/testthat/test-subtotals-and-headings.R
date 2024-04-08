context("Subtotals and headings")

# Skip tests on windows (because they're slow and CRAN complains)
if (tolower(Sys.info()[["sysname"]]) != "windows") {
    test_that("Subtotal accepts a variety of inputs", {
        subtot1 <- Subtotal(name = "Approval", categories = c(1, 2), after = 2)
        expect_true(is.Subtotal(subtot1))

        subtot2 <- Subtotal(name = "Approval", categories = list(1, 2), after = 2)
        expect_true(is.Subtotal(subtot2))

        subtot3 <- Subtotal(name = "Approval", categories = list(1:2), after = 2)
        expect_true(is.Subtotal(subtot3))

        expect_equal(subtot1, subtot2)
        expect_equal(subtot2, subtot3)

        subtot_top <- Subtotal(name = "Approval", categories = c(1, 2), position = "top")
        subtot_bottom <- Subtotal(name = "Approval", categories = c(1, 2), position = "bottom")

        subtot_after_last_cat <- Subtotal(name = "Approval", categories = c(1, 2))
        expect_message(
            expect_equal(anchor(subtot_after_last_cat), NA_integer_),
            paste0(
                "Can't determine the anchor position without a ",
                "variable. However, when this is added to a Crunch ",
                "variable or CrunchCube it will follow the last ",
                "category given"
            )
        )
    })

    test_that("Subtotal validates", {
        expect_error(
            Subtotal(categories = c(1, 2), after = 2),
            "argument \"name\" is missing, with no default"
        )
        expect_error(
            Subtotal(name = "Total Approval", after = 2),
            "Must specify at least one of categories or negative for a valid Subtotal"
        )

        expect_error(
            Subtotal(name = "Total Approval", categories = 1, before = 1, after = 2),
            "Cannot specify both the .*after.* and .*before.* arguments"
        )
    })

    test_that("Subtotal validates with fixed positions", {
        expect_error(
            Subtotal(categories = c(1, 2), after = 2, position = "top"),
            paste0(
                "If position is not relative, you cannot supply a category id ",
                "or name to the .*after.* argument"
            )
        )
        expect_error(
            Subtotal(
                name = "Total Approval", categories = c(1, 2),
                position = "not a position"
            ),
            "'arg' should be one of .*relative.*, .*top.*, .*bottom.*"
        )
    })

    test_that("Heading validates", {
        expect_error(
            Heading(after = 2),
            "argument \"name\" is missing, with no default"
        )
        expect_error(Heading(name = "All the approves", after = 2, categories = c(1, 2)),
                     "unused argument (categories = c(1, 2))",
                     fixed = TRUE
        )
    })

    subtot <- Subtotal(name = "a subtotal", after = 1, categories = c(1, 2))
    heading <- Heading(name = "a heading", position = "top")

    test_that("Subtotal/Heading getters", {
        expect_equal(func(subtot), "subtotal")
        expect_equal(func(heading), NA)
        expect_equal(arguments(heading), NA)
    })

    test_that("Subtotal/Heading setters", {
        # names
        name(subtot) <- "new name"
        expect_equal(name(subtot), "new name")
        name(heading) <- "new head"
        expect_equal(name(heading), "new head")

        # arguments
        arguments(subtot) <- c(2, 3)
        expect_equal(arguments(subtot), c(2, 3))
        expect_error(
            arguments(heading) <- c(2, 3),
            "Cannot set arguments on Headings."
        )

        # anchors
        anchor(subtot) <- 2
        expect_equal(anchor(subtot), 2)
        expect_equal(subtot$position, "relative")
        anchor(subtot) <- "bottom"
        # anchor grabs position when after is null
        expect_equal(anchor(subtot), "bottom")
        expect_equal(subtot$position, "bottom")
        # after is null when position is anything other than relative
        expect_null(subtot$after)

        anchor(heading) <- 1
        expect_equal(anchor(heading), 1)
        expect_equal(heading$position, "relative")
    })

    with_mock_crunch({
        ds <- cachedLoadDataset("test ds")

        test_that("subtotals retrieves the subtotals and headings Insertions", {
            expect_equivalent(
                subtotals(ds$location),
                Insertions(data = list(
                    Subtotal(
                        name = "London+Scotland", after = 3,
                        categories = c(1, 2)
                    )
                ))
            )
        })

        test_that("can update a subtotals' name, arguments, and anchor", {
            expect_PATCH(
                anchor(subtotals(ds$location)[[1]]) <- 1,
                "https://app.crunch.io/api/datasets/1/variables/location/",
                '{"view":{"transform":',
                '{"insertions":[{"anchor":1,"name":"London+Scotland",',
                '"function":"subtotal","args":[1,2],"kwargs":{"positive":[1,2]},"id":1}]}}}'
            )
            expect_PATCH(
                name(subtotals(ds$location)[[1]]) <- "new name",
                "https://app.crunch.io/api/datasets/1/variables/location/",
                '{"view":{"transform":',
                '{"insertions":[{"anchor":3,"name":"new name",',
                '"function":"subtotal","args":[1,2],"kwargs":{"positive":[1,2]},"id":1}]}}}'
            )
            expect_PATCH(
                arguments(subtotals(ds$location)[[1]]) <- c(2, 3),
                "https://app.crunch.io/api/datasets/1/variables/location/",
                '{"view":{"transform":',
                '{"insertions":[{"anchor":3,"name":"London+Scotland",',
                '"function":"subtotal","args":[2,3],"kwargs":{"positive":[2,3]},"id":1}]}}}'
            )
        })

        test_that("subtotals returns null when there are no subtotals", {
            expect_null(subtotals(ds$gender))
        })

        test_that("Assigning NULL if already NULL does nothing", {
            expect_no_request(subtotals(ds$gender) <- NULL)
        })

        test_that("Adding subtotals and headers to a variable that has none, works", {
            expect_PATCH(
                subtotals(ds$gender) <- list(
                    Subtotal(name = "Not men", categories = c(1, -1), after = "Female"),
                    Subtotal(name = "Women", categories = "Female", after = 4),
                    Heading(name = "A subtitle", position = "top")
                ),
                "https://app.crunch.io/api/datasets/1/variables/gender/",
                '{"view":{"transform":{"insertions":[',
                '{"anchor":2,"name":"Not men","function":"subtotal","args":[1,-1],"kwargs":',
                '{"positive":[1,-1]},"id":1},',
                '{"anchor":4,"name":"Women","function":"subtotal","args":[2],"kwargs":',
                '{"positive":[2]},"id":2},',
                '{"anchor":"top","name":"A subtitle","id":3}]}}}'
            )
        })

        test_that("Can add subtotal difference", {
            expect_PATCH(
                subtotals(ds$gender) <- list(
                    Subtotal(name = "W-M", categories = 2, after = "Female", negative = 1)
                ),
                "https://app.crunch.io/api/datasets/1/variables/gender/",
                '{"view":{"transform":{"insertions":[',
                '{"anchor":2,"name":"W-M","function":"subtotal","args":[2],"kwargs":',
                '{"positive":[2],"negative":[1]},"id":1}]}}}'
            )
        })

        test_that("When after is not supplied, it defauls to follow the last category", {
            expect_PATCH(
                subtotals(ds$gender) <- list(
                    # categories supplied in reverse order
                    Subtotal(name = "Not men", categories = c(-1, 1)),
                    Subtotal(name = "Women", categories = "Female")
                ),
                "https://app.crunch.io/api/datasets/1/variables/gender/",
                '{"view":{"transform":{"insertions":[',
                '{"anchor":-1,"name":"Not men","function":"subtotal","args":[-1,1],"kwargs":',
                '{"positive":[-1,1]},"id":1},',
                '{"anchor":2,"name":"Women","function":"subtotal","args":[2],"kwargs":',
                '{"positive":[2]},"id":2}]}}}'
            )

            # one supplied category (23) isn't a real category, after should still be -1
            expect_PATCH(
                subtotals(ds$gender) <- list(
                    # categories supplied in reverse order
                    Subtotal(name = "Not men", categories = c(-1, 1, 23)),
                    Subtotal(name = "Women", categories = "Female")
                ),
                "https://app.crunch.io/api/datasets/1/variables/gender/",
                '{"view":{"transform":{"insertions":[',
                '{"anchor":-1,"name":"Not men","function":"subtotal","args":[-1,1,23],"kwargs":',
                '{"positive":[-1,1,23]},"id":1},',
                '{"anchor":2,"name":"Women","function":"subtotal","args":[2],"kwargs":',
                '{"positive":[2]},"id":2}]}}}'
            )
        })

        test_that("subtotals and headers to a variable that has some, appends", {
            expect_PATCH(
                subtotals(ds$location) <- list(
                    Subtotal(name = "London alone", categories = c(1), after = "London"),
                    Subtotal(name = "Scotland alone", categories = "Scotland", after = 2),
                    Heading(name = "A subtitle", position = "top")
                ),
                "https://app.crunch.io/api/datasets/1/variables/location/",
                '{"view":{"transform":{"insertions":[',
                '{"anchor":1,"name":"London alone","function":"subtotal","args":[1],"kwargs":',
                '{"positive":[1]},"id":1},',
                '{"anchor":2,"name":"Scotland alone","function":"subtotal","args":[2],"kwargs":',
                '{"positive":[2]},"id":2},',
                '{"anchor":"top","name":"A subtitle","id":3}]}}}'
            )
        })

        test_that("subtotals don't duplicate, they replace", {
            expect_PATCH(
                subtotals(ds$location) <- list(
                    Subtotal(
                        name = "London+Scotland", categories = c(2, 1),
                        position = "top"
                    )
                ),
                "https://app.crunch.io/api/datasets/1/variables/location/",
                '{"view":{"transform":{"insertions":[',
                '{"anchor":"top","name":"London+Scotland","function":"subtotal","args":',
                '[2,1],"kwargs":{"positive":[2,1]},"id":1}]}}}'
            )
        })

        test_that("headings don't duplicate, they replace", {
            expect_PATCH(
                subtotals(ds$location) <- list(
                    Heading(name = "London+Scotland", after = 1)
                ),
                "https://app.crunch.io/api/datasets/1/variables/location/",
                '{"view":{"transform":{"insertions":[',
                '{"anchor":1,"name":"London+Scotland","id":1}]}}}'
            )
        })

        test_that("bare subtotals and headers can be added", {
            expect_PATCH(
                subtotals(ds$location) <-
                    Subtotal(name = "London alone", categories = c(1), after = "London"),
                "https://app.crunch.io/api/datasets/1/variables/location/",
                '{"view":{"transform":{"insertions":[',
                '{"anchor":1,"name":"London alone","function":"subtotal","args":[1],"kwargs":',
                '{"positive":[1]},"id":1}]}}}'
            )

            expect_PATCH(
                subtotals(ds$location) <-
                    Heading(name = "A subtitle", position = "top"),
                "https://app.crunch.io/api/datasets/1/variables/location/",
                '{"view":{"transform":{"insertions":[',
                '{"anchor":"top","name":"A subtitle","id":1}]}}}'
            )
        })

        test_that("subtotals and headers can be removed", {
            expect_PATCH(
                subtotals(ds$location) <- NULL,
                "https://app.crunch.io/api/datasets/1/variables/location/",
                '{"view":{"transform":{"insertions":[]}}}'
            )
        })

        test_that("subtotals respect ids if provided", {
            expect_PATCH(
                subtotals(ds$location) <- list(
                    Subtotal(name = "London alone", categories = c(1), after = "London"),
                    Subtotal(name = "Scotland alone", categories = "Scotland", after = 2, id = 1)
                ),
                "https://app.crunch.io/api/datasets/1/variables/location/",
                '{"view":{"transform":{"insertions":[',
                '{"anchor":1,"name":"London alone","function":"subtotal","args":[1],"kwargs":',
                '{"positive":[1]},"id":2},',
                '{"anchor":2,"name":"Scotland alone","function":"subtotal","args":[2],"kwargs":',
                '{"positive":[2]},"id":1}]}}}'
            )
        })

        test_that("can add MR Subtotals", {
            expect_PATCH(
                subtotals(ds$mymrset) <- list(
                    Subtotal(name = "s1 or s2", c("subvar1", "subvar2"), position = "top")
                ),
                "https://app.crunch.io/api/datasets/1/variables/mymrset/",
                '{"view":{"transform":{"insertions":[{"anchor":"top","name":"s1 or s2",',
                '"function":"any_non_missing_selected","kwargs":{"variable":',
                '"mymrset","subvariable_ids":["subvar1","subvar2"]},"id":1}]}}}'
            )
        })

        test_that("can add MR Subtotals with after position", {
            expect_PATCH(
                subtotals(ds$mymrset) <- list(
                    Subtotal(name = "s1 or s2", c("subvar1", "subvar2"), after = "subvar1")
                ),
                "https://app.crunch.io/api/datasets/1/variables/mymrset/",
                '{"view":{"transform":{"insertions":[{"anchor":{"position":"after","alias":',
                '"subvar1"},"name":"s1 or s2","function":"any_non_missing_selected","kwargs":',
                '{"variable":"mymrset","subvariable_ids":["subvar1","subvar2"]},"id":1}]}}}'
            )
        })

        test_that("can add MR Subtotals with before position", {
            expect_PATCH(
                subtotals(ds$mymrset) <- list(
                    Subtotal(name = "s1 or s2", c("subvar1", "subvar2"), before = "subvar1")
                ),
                "https://app.crunch.io/api/datasets/1/variables/mymrset/",
                '{"view":{"transform":{"insertions":[{"anchor":{"position":"before","alias":',
                '"subvar1"},"name":"s1 or s2","function":"any_non_missing_selected","kwargs":',
                '{"variable":"mymrset","subvariable_ids":["subvar1","subvar2"]},"id":1}]}}}'
            )
        })

        test_that("can add MR Subtotals with na.rm=FALSE", {
            expect_PATCH(
                subtotals(ds$mymrset) <- list(
                    Subtotal(
                        name = "s1 or s2", c("subvar1", "subvar2"), position = "top", na.rm = FALSE
                    )
                ),
                "https://app.crunch.io/api/datasets/1/variables/mymrset/",
                '{"view":{"transform":{"insertions":[{"anchor":"top","name":"s1 or s2",',
                '"function":"any_selected","kwargs":{"variable":',
                '"mymrset","subvariable_ids":["subvar1","subvar2"]},"id":1}]}}}'
            )
        })

        test_that("can add MR Subtotals with subvar names", {
            expect_PATCH(
                subtotals(ds$mymrset) <- list(
                    # Confusing thing: name="Second" matches alias="subvar1"
                    Subtotal(name = "s1 or s2", c("Second", "First"), after = "Second")
                ),
                "https://app.crunch.io/api/datasets/1/variables/mymrset/",
                '{"view":{"transform":{"insertions":[{"anchor":{"position":"after","alias":',
                '"subvar1"},"name":"s1 or s2","function":"any_non_missing_selected","kwargs":',
                '{"variable":"mymrset","subvariable_ids":["subvar1","subvar2"]},"id":1}]}}}'
            )
        })

        test_that("can add MR Subtotals with default position", {
            expect_PATCH(
                subtotals(ds$mymrset) <- list(
                    Subtotal(name = "s1 or s2", c("subvar1", "subvar2"))
                ),
                "https://app.crunch.io/api/datasets/1/variables/mymrset/",
                '{"view":{"transform":{"insertions":[{"anchor":{"position":"after","alias":',
                '"subvar1"},"name":"s1 or s2","function":"any_non_missing_selected","kwargs":',
                '{"variable":"mymrset","subvariable_ids":["subvar1","subvar2"]},"id":1}]}}}'
            )
        })

        test_that("can add MR Subtotals with explicit alias", {
            expect_PATCH(
                subtotals(ds$mymrset) <- list(
                    Subtotal(
                        name = "s1 or s2",
                        c("subvar1", "subvar2"),
                        position = "top",
                        alias = "top2"
                    )
                ),
                "https://app.crunch.io/api/datasets/1/variables/mymrset/",
                '{"view":{"transform":{"insertions":[{"anchor":"top","name":"s1 or s2",',
                '"function":"any_non_missing_selected","kwargs":{"variable":',
                '"mymrset","subvariable_ids":["subvar1","subvar2"]},"alias":"top2","id":1}]}}}'
            )
        })

        test_that("reasonable error when MR subvariables don't match aliases nor names", {
            expect_error(
                subtotals(ds$mymrset) <- list(
                    Subtotal(name = "bad", c("ABC", "subvar2"), position = "top")
                ),
                "`subvariable_ids` must be all aliases or all names, but"
            )
        })

        test_that("can use subtypeInsertion on handcrafted MR insertions", {
            inserts <- Insertions(Insertion(
                anchor = "top",
                `function` = "any_selected",
                name = "s1 or s2",
                id = 1L,
                kwargs = list(
                    variable = "mymrset",
                    subvariable_ids = c("subvar1", "subvar2")
                ),
                alias = NULL
            ))

            expect_equal(
                subtypeInsertions(inserts),
                Insertions(Subtotal(
                    "s1 or s2",
                    c("subvar1", "subvar2"),
                    position = "top",
                    id = 1L,
                    variable = "mymrset"
                ))
            )


        })


        ds_veg <- cachedLoadDataset("Vegetables example") ## Has MR insertions
        test_that("can print an mr subtotal", {
            expect_prints(
                subtotals(ds_veg$funnel_aware_mr),
                get_output(data.frame(
                    anchor = c("top"),
                    name = c("Jicama or Kohlrabi"),
                    func = c("subtotal"),
                    args = c("funnel_aware_mr_1 and funnel_aware_mr_2"),
                    kwargs = c(""),
                    stringsAsFactors = FALSE
                )),
                fixed = TRUE
            )
        })
    })


    with_test_authentication({
        ds <- flakyRecoverNewDataset(df)

        trans <- Transforms(insertions = list(
            Subtotal(
                position = "top",
                name = "B-C",
                categories = 1,
                negative = 2
            ),
            Subtotal(
                after = 1, name = "B alone",
                categories = c(1)
            ),
            Subtotal(
                after = 2, name = "C alone",
                categories = c(2)
            ),
            Subtotal(
                position = "bottom", name = "B+C",
                categories = c(1, 2)
            )
        ))
        trans_resp <- trans
        trans_resp["categories"] <- list(NULL)
        trans_resp["elements"] <- list(NULL)

        test_that("Can get and set headings and subtotals", {
            expect_null(transforms(ds$v4))

            subtotals(ds$v4) <- list(
                Subtotal(
                    position = "top",
                    name = "B-C",
                    categories = 1,
                    negative = 2
                ),
                Subtotal(
                    name = "B alone", categories = c("B"),
                    after = 1
                ),
                Subtotal(
                    name = "C alone", categories = c(2),
                    after = "C"
                ),
                Subtotal(
                    name = "B+C", categories = c(1, 2),
                    position = "bottom"
                )
            )

            expect_json_equivalent(transforms(ds$v4), trans_resp)

            expect_prints(subtotals(ds$v4),
                          get_output(data.frame(
                              anchor = c("top", 1, 2, "bottom"),
                              name = c("B-C", "B alone", "C alone", "B+C"),
                              func = c("subtotal", "subtotal", "subtotal", "subtotal"),
                              args = c("1", "1", "2", "1 and 2"),
                              kwargs = c(
                                  "positive: 1 | negative: 2",
                                  "positive: 1 | ",
                                  "positive: 2 | ",
                                  "positive: 1 and 2 | "
                              ),
                              stringsAsFactors = FALSE
                          )),
                          fixed = TRUE
            )

            # check shape
            v4_ary <- array(c(0, 10, 10, 10, 10, 20),
                            dimnames = list(c(
                                "B-C", "B", "B alone",
                                "C", "C alone", "B+C"
                            ))
            )
            # capture.output so that we don't print during the test.
            capture.output(v4_with_trans <- showTransforms(ds$v4))

            expect_equivalent(v4_with_trans, v4_ary)
        })

        test_that("Can modify subtotals in place", {
            # assert known shape
            expect_equal(names(subtotals(ds$v4)), c(
                "B-C", "B alone",
                "C alone", "B+C"
            ))
            expect_equal(anchors(subtotals(ds$v4)), c("top", 1, 2, "bottom"))
            expect_equal(arguments(subtotals(ds$v4)[[1]]), 1)
            expect_equal(arguments(subtotals(ds$v4)[[2]]), 1)
            expect_equal(arguments(subtotals(ds$v4)[[3]]), 2)
            expect_equal(arguments(subtotals(ds$v4)[[4]]), c(1, 2))

            # changing names
            name(subtotals(ds$v4)[[2]]) <- "C and B"
            expect_equal(name(subtotals(ds$v4)[[2]]), "C and B")

            # changing anchors
            anchor(subtotals(ds$v4)[[2]]) <- 2
            expect_equal(anchor(subtotals(ds$v4)[[2]]), 2)
            anchor(subtotals(ds$v4)[[2]]) <- "B"
            expect_equal(anchor(subtotals(ds$v4)[[2]]), 1)
            anchor(subtotals(ds$v4)[[3]]) <- 1
            expect_equal(anchor(subtotals(ds$v4)[[3]]), 1)
            anchor(subtotals(ds$v4)[[1]]) <- "bottom"
            expect_equal(anchor(subtotals(ds$v4)[[1]]), "bottom")

            # changing arguments
            arguments(subtotals(ds$v4)[[2]]) <- c(1, 2)
            expect_equal(arguments(subtotals(ds$v4)[[2]]), c(1, 2))

            arguments(subtotals(ds$v4)[[2]]) <- c("C", "B")
            expect_equal(arguments(subtotals(ds$v4)[[2]]), c(2, 1))

            # refresh to ensure that the changes have stuck
            ds <- refresh(ds)
            expect_equal(names(subtotals(ds$v4)), c("B-C", "C and B", "C alone", "B+C"))
            expect_equal(anchors(subtotals(ds$v4)), c("bottom", 1, 1, "bottom"))
            expect_equal(arguments(subtotals(ds$v4)[[1]]), 1)
            expect_equal(arguments(subtotals(ds$v4)[[2]]), c(2, 1))
            expect_equal(arguments(subtotals(ds$v4)[[3]]), 2)
            expect_equal(arguments(subtotals(ds$v4)[[4]]), c(1, 2))
        })
    })
}
