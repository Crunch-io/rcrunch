context("Subtotals and headings")

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
        "argument \"categories\" is missing, with no default"
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
    ds <- loadDataset("test ds")

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
            '"function":"subtotal","args":[1,2]}]}}}'
        )
        expect_PATCH(
            name(subtotals(ds$location)[[1]]) <- "new name",
            "https://app.crunch.io/api/datasets/1/variables/location/",
            '{"view":{"transform":',
            '{"insertions":[{"anchor":3,"name":"new name",',
            '"function":"subtotal","args":[1,2]}]}}}'
        )
        expect_PATCH(
            arguments(subtotals(ds$location)[[1]]) <- c(2, 3),
            "https://app.crunch.io/api/datasets/1/variables/location/",
            '{"view":{"transform":',
            '{"insertions":[{"anchor":3,"name":"London+Scotland",',
            '"function":"subtotal","args":[2,3]}]}}}'
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
            '{"anchor":2,"name":"Not men","function":"subtotal","args":[1,-1]},',
            '{"anchor":4,"name":"Women","function":"subtotal","args":[2]},',
            '{"anchor":"top","name":"A subtitle"}]}}}'
        )
    })

    test_that("When after is not supplied, it defauls to follow the last category", {
        expect_PATCH(
            subtotals(ds$gender) <- list(
                Subtotal(name = "Not men", categories = c(-1, 1)), # categories supplied in reverse order
                Subtotal(name = "Women", categories = "Female")
            ),
            "https://app.crunch.io/api/datasets/1/variables/gender/",
            '{"view":{"transform":{"insertions":[',
            '{"anchor":-1,"name":"Not men","function":"subtotal","args":[-1,1]},',
            '{"anchor":2,"name":"Women","function":"subtotal","args":[2]}]}}}'
        )

        # one supplied category (23) isn't a real category, after should still be -1
        expect_PATCH(
            subtotals(ds$gender) <- list(
                Subtotal(name = "Not men", categories = c(-1, 1, 23)), # categories supplied in reverse order
                Subtotal(name = "Women", categories = "Female")
            ),
            "https://app.crunch.io/api/datasets/1/variables/gender/",
            '{"view":{"transform":{"insertions":[',
            '{"anchor":-1,"name":"Not men","function":"subtotal","args":[-1,1,23]},',
            '{"anchor":2,"name":"Women","function":"subtotal","args":[2]}]}}}'
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
            '{"anchor":1,"name":"London alone","function":"subtotal","args":[1]},',
            '{"anchor":2,"name":"Scotland alone","function":"subtotal","args":[2]},',
            '{"anchor":"top","name":"A subtitle"}]}}}'
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
            "[2,1]}]}}}"
        )
    })

    test_that("headings don't duplicate, they replace", {
        expect_PATCH(
            subtotals(ds$location) <- list(
                Heading(name = "London+Scotland", after = 1)
            ),
            "https://app.crunch.io/api/datasets/1/variables/location/",
            '{"view":{"transform":{"insertions":[',
            '{"anchor":1,"name":"London+Scotland"}]}}}'
        )
    })

    test_that("bare subtotals and headers can be added", {
        expect_PATCH(
            subtotals(ds$location) <-
                Subtotal(name = "London alone", categories = c(1), after = "London"),
            "https://app.crunch.io/api/datasets/1/variables/location/",
            '{"view":{"transform":{"insertions":[',
            '{"anchor":1,"name":"London alone","function":"subtotal","args":[1]}]}}}'
        )

        expect_PATCH(
            subtotals(ds$location) <-
                Heading(name = "A subtitle", position = "top"),
            "https://app.crunch.io/api/datasets/1/variables/location/",
            '{"view":{"transform":{"insertions":[',
            '{"anchor":"top","name":"A subtitle"}]}}}'
        )
    })

    test_that("subtotals and headers can be removed", {
        expect_PATCH(
            subtotals(ds$location) <- NULL,
            "https://app.crunch.io/api/datasets/1/variables/location/",
            '{"view":{"transform":{"insertions":[]}}}'
        )
    })

    test_that("subtotals and headers can be removed", {
        expect_error(
            subtotals(ds$location) <- list("1", "2"),
            "value must be a list of Subtotals, Headings, or both."
        )
    })
})


with_test_authentication({
    ds <- newDataset(df)

    trans <- Transforms(insertions = list(
        Heading(position = "top", name = "This is a subtitle"),
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
            Heading(name = "This is a subtitle", position = "top"),
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
                name = c("This is a subtitle", "B alone", "C alone", "B+C"),
                func = c(NA, "subtotal", "subtotal", "subtotal"),
                args = c("NA", "1", "2", "1 and 2"),
                stringsAsFactors = FALSE
            )),
            fixed = TRUE
        )

        # check shape
        v4_ary <- array(c(NA, 10, 10, 10, 10, 20),
            dimnames = list(c(
                "This is a subtitle", "B", "B alone",
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
            "This is a subtitle", "B alone",
            "C alone", "B+C"
        ))
        expect_equal(anchors(subtotals(ds$v4)), c("top", 1, 2, "bottom"))
        expect_equal(arguments(subtotals(ds$v4)[[1]]), NA)
        expect_equal(arguments(subtotals(ds$v4)[[2]]), 1)
        expect_equal(arguments(subtotals(ds$v4)[[3]]), 2)
        expect_equal(arguments(subtotals(ds$v4)[[4]]), c(1, 2))

        # changing names
        name(subtotals(ds$v4)[[1]]) <- "The new subtitle"
        expect_equal(name(subtotals(ds$v4)[[1]]), "The new subtitle")
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
        expect_equal(names(subtotals(ds$v4)), c("The new subtitle", "C and B", "C alone", "B+C"))
        expect_equal(anchors(subtotals(ds$v4)), c("bottom", 1, 1, "bottom"))
        expect_equal(arguments(subtotals(ds$v4)[[1]]), NA)
        expect_equal(arguments(subtotals(ds$v4)[[2]]), c(2, 1))
        expect_equal(arguments(subtotals(ds$v4)[[3]]), 2)
        expect_equal(arguments(subtotals(ds$v4)[[4]]), c(1, 2))
    })
})
