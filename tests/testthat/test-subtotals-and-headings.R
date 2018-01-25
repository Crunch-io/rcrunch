context("Subtotals and headings ")

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
    subtot_top <- Subtotal(name = "Approval", categories = c(1, 2), position = "bottom")
})

test_that("Subtotal validates", {
    expect_error(Subtotal(categories = c(1, 2), after = 2),
                 "argument \"name\" is missing, with no default")
    expect_error(Subtotal(name = "Total Approval", after = 2),
                 "argument \"categories\" is missing, with no default")
    expect_error(Subtotal(name = "Total Approval", categories = c(1, 2)),
                 paste0("If position is relative, you must supply a category ",
                        "id or name to the .*after.* argument"))
})

test_that("Subtotal validates with fixed positions", {
    expect_error(Subtotal(categories = c(1, 2), after = 2, position = "top"),
        paste0("If position is not relative, you cannot supply a category id ",
               "or name to the .*after.* argument"))
    expect_error(Subtotal(name = "Total Approval", categories = c(1, 2),
        position = "not a position"),
        "'arg' should be one of .*relative.*, .*top.*, .*bottom.*")

})

test_that("Heading validates", {
    expect_error(Heading(after = 2),
                 "argument \"name\" is missing, with no default")
    expect_error(Heading(name = "All the approves"),
                 paste0("If position is relative, you must supply a category ",
                        "id or name to the .*after.* argument"))
    expect_error(Heading(name = "All the approves", after = 2, categories = c(1, 2)),
                 "unused argument (categories = c(1, 2))", fixed = TRUE)
})

test_that("Subtotal/Heading getters", {
    subtot <- Subtotal(name = "a subtotal", after = 1, categories = c(1, 2))
    heading <- Heading(name = "a heading", position = "top")

    expect_equal(func(subtot), "subtotal")
    expect_equal(func(heading), NA)
    expect_equal(args(heading), NA)
})


with_mock_crunch({
    ds <- loadDataset("test ds")

    test_that("subtotals retrieves the subtotals and headings Insertions", {
        expect_equivalent(subtotals(ds$location),
                          Insertions(data=list(
                              Subtotal(name = "London+Scotland", after = 3,
                                   categories = c(1, 2))))
        )
    })

    test_that("subtotals returns null when there are no subtotals", {
        expect_null(subtotals(ds$gender))
    })

    test_that("Adding subtotals and headers to a variable that has none, works", {
        expect_PATCH(subtotals(ds$gender) <- list(
            Subtotal(name = "Not men", categories = c(1, -1), after = "Female"),
            Subtotal(name = "Women", categories = "Female", after = 4),
            Heading(name = "A subtitle", position = "top")
        ),
        'https://app.crunch.io/api/datasets/1/variables/gender/',
        '{"element":"shoji:entity","body":{"view":{"transform":{"insertions":[',
        '{"anchor":2,"name":"Not men","function":"subtotal","args":[1,-1]},',
        '{"anchor":4,"name":"Women","function":"subtotal","args":[2]},',
        '{"anchor":"top","name":"A subtitle"}]}}}}')
    })

    test_that("subtotals and headers to a variable that has some, appends", {
        expect_PATCH(subtotals(ds$location) <- list(
            Subtotal(name = "London alone", categories = c(1), after = "London"),
            Subtotal(name = "Scotland alone", categories = "Scotland", after = 2),
            Heading(name = "A subtitle", position = "top")
        ),
        'https://app.crunch.io/api/datasets/1/variables/location/',
        '{"element":"shoji:entity","body":{"view":{"transform":{"insertions":[',
        '{"anchor":3,"name":"London+Scotland","function":"subtotal","args":',
        '[1,2]},',
        '{"anchor":1,"name":"London alone","function":"subtotal","args":[1]},',
        '{"anchor":2,"name":"Scotland alone","function":"subtotal","args":[2]},',
        '{"anchor":"top","name":"A subtitle"}]}}}}')
    })

    test_that("subtotals don't duplicate, they replace", {
        expect_PATCH(subtotals(ds$location) <- list(
            Subtotal(name = "London+Scotland", categories = c(2,1),
                     position = "top")),
        'https://app.crunch.io/api/datasets/1/variables/location/',
        '{"element":"shoji:entity","body":{"view":{"transform":{"insertions":[',
        '{"anchor":"top","name":"London+Scotland","function":"subtotal","args":',
        '[2,1]}]}}}}')
    })

    test_that("headings don't duplicate, they replace", {
        expect_PATCH(subtotals(ds$location) <- list(
            Heading(name = "London+Scotland", after = 1)),
            'https://app.crunch.io/api/datasets/1/variables/location/',
            '{"element":"shoji:entity","body":{"view":{"transform":{"insertions":[',
            '{"anchor":1,"name":"London+Scotland"}]}}}}')
    })

    test_that("bare subtotals and headers can be added", {
        expect_PATCH(subtotals(ds$location) <-
            Subtotal(name = "London alone", categories = c(1), after = "London"),
        'https://app.crunch.io/api/datasets/1/variables/location/',
        '{"element":"shoji:entity","body":{"view":{"transform":{"insertions":[',
        '{"anchor":3,"name":"London+Scotland","function":"subtotal","args":',
        '[1,2]},',
        '{"anchor":1,"name":"London alone","function":"subtotal","args":[1]}]}}}}')

        expect_PATCH(subtotals(ds$location) <-
            Heading(name = "A subtitle", position = "top"),
        'https://app.crunch.io/api/datasets/1/variables/location/',
        '{"element":"shoji:entity","body":{"view":{"transform":{"insertions":[',
        '{"anchor":3,"name":"London+Scotland","function":"subtotal","args":',
        '[1,2]},',
        '{"anchor":"top","name":"A subtitle"}]}}}}')
    })

    test_that("subtotals and headers can be removed", {
        expect_PATCH(subtotals(ds$location) <- NULL,
        'https://app.crunch.io/api/datasets/1/variables/location/',
        '{"element":"shoji:entity","body":{"view":{"transform":{"insertions":[]}}}}')
    })

    test_that("subtotals and headers can be removed", {
        expect_error(subtotals(ds$location) <- list("1", "2"),
                     "value must be a list of Subtotals, Headings, or both.")
    })
})


with_test_authentication({
    ds <- newDataset(df)

    test_that("Can get and set headings and subtotals", {
        trans <- Transforms(insertions = list(
            Heading(position = "top", name = "This is a subtitle"),
            Subtotal(after = 1, name = "B alone",
                 categories = c(1)),
            Subtotal(after = 2, name = "C alone",
                 categories = c(2)),
            Subtotal(position = "bottom", name = "B+C",
                 categories = c(1, 2))))
        expect_null(transforms(ds$v4))
        subtotals(ds$v4) <- list(Heading(name = "This is a subtitle", position = "top"),
                                 Subtotal(name = "B alone", categories = c("B"),
                                          after = 1),
                                 Subtotal(name = "C alone", categories = c(2),
                                          after = "C"),
                                 Subtotal(name = "B+C", categories = c(1, 2),
                                          position = "bottom"))
        trans_resp <- trans
        trans_resp["categories"] <- list(NULL)
        trans_resp["elements"] <- list(NULL)

        expect_json_equivalent(transforms(ds$v4), trans_resp)

        # check shape
        v4_ary <- array(c(NA, 10, 10, 10, 10, 20),
                        dimnames = list(c("This is a subtitle", "B", "B alone",
                                          "C", "C alone",  "B+C")))
        # capture.output so that we don't print during the test.
        capture.output(v4_with_trans <- showTransforms(ds$v4))

        expect_equivalent(v4_with_trans, v4_ary)
    })
})
