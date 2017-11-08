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
})

test_that("Subtotal validates", {
    expect_error(Subtotal(categories = c(1, 2), after = 2),
                 "A Subtotal must have at least .* Missing: .*name.*")
    expect_error(Subtotal(name = "Total Approval", after = 2),
                 "A Subtotal must have at least .* Missing: .*categories.*")
    expect_error(Subtotal(name = "Total Approval", categories = c(1, 2)),
                 "A Subtotal must have at least .* Missing: .*after.*")
})

test_that("Heading validates", {
    expect_error(Heading(after = 2),
                 "A Heading must have at least .* Missing: .*name.*")
    expect_error(Heading(name = "All the approves"),
                 "A Heading must have at least .* Missing: .*after.*")
    expect_error(Heading(name = "All the approves", after = 2, categories = c(1, 2)),
                 "A Heading cannot have .*categories.*. Did you mean to make a Subtotal?")
})

with_mock_crunch({
    ds <- loadDataset("test ds")

    test_that("subtotals retrievs the subtotals and headings Insertions", {
        expect_equivalent(subtotals(ds$location),
                          Insertions(data=list(
                              list(anchor = "3", name = "London+Scotland",
                                   `function` = "subtotal", args = c("1", "2"))))
        )
    })

    test_that("Adding subtotals and headers to a variable that has none, works", {
        expect_PATCH(subtotals(ds$gender) <- list(
            Subtotal(name = "Not men", categories = c(1, -1), after = "Female"),
            Subtotal(name = "Women", categories = "Female", after = 4),
            Heading(name = "A subtitle", after = 0)
        ),
        'https://app.crunch.io/api/datasets/1/variables/gender/',
        '{"element":"shoji:entity","body":{"view":{"transform":{"insertions":[',
        '{"anchor":2,"name":"Not men","function":"subtotal","args":[1,-1]},',
        '{"anchor":4,"name":"Women","function":"subtotal","args":[2]},',
        '{"anchor":0,"name":"A subtitle"}]}}}}')
    })

    test_that("subtotals and headers to a variable that has some, appends", {
        expect_PATCH(subtotals(ds$location) <- list(
            Subtotal(name = "London alone", categories = c(1), after = "London"),
            Subtotal(name = "Scotland alone", categories = "Scotland", after = 2),
            Heading(name = "A subtitle", after = 0)
        ),
        'https://app.crunch.io/api/datasets/1/variables/location/',
        '{"element":"shoji:entity","body":{"view":{"transform":{"insertions":[',
        '{"anchor":"3","name":"London+Scotland","function":"subtotal","args":',
        '["1","2"]},',
        '{"anchor":1,"name":"London alone","function":"subtotal","args":[1]},',
        '{"anchor":2,"name":"Scotland alone","function":"subtotal","args":[2]},',
        '{"anchor":0,"name":"A subtitle"}]}}}}')
    })

    test_that("subtotals don't duplicate, they replace", {
        expect_PATCH(subtotals(ds$location) <- list(
            Subtotal(name = "London+Scotland", categories = c(2,1), after = 0)),
        'https://app.crunch.io/api/datasets/1/variables/location/',
        '{"element":"shoji:entity","body":{"view":{"transform":{"insertions":[',
        '{"anchor":0,"name":"London+Scotland","function":"subtotal","args":',
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
        '{"anchor":"3","name":"London+Scotland","function":"subtotal","args":',
        '["1","2"]},',
        '{"anchor":1,"name":"London alone","function":"subtotal","args":[1]}]}}}}')

        expect_PATCH(subtotals(ds$location) <-
            Heading(name = "A subtitle", after = 0),
        'https://app.crunch.io/api/datasets/1/variables/location/',
        '{"element":"shoji:entity","body":{"view":{"transform":{"insertions":[',
        '{"anchor":"3","name":"London+Scotland","function":"subtotal","args":',
        '["1","2"]},',
        '{"anchor":0,"name":"A subtitle"}]}}}}')
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
            list(anchor = 0, name = "This is a subtitle"),
            list(anchor = 1, name = "B alone",
                 `function` = "subtotal", args = c(1)),
            list(anchor = 2, name = "C alone",
                 `function` = "subtotal", args = c(2)),
            list(anchor = 999, name = "B+C",
                 `function` = "subtotal", args = c(1, 2))))
        expect_null(transforms(ds$v4))
        subtotals(ds$v4) <- list(Heading(name = "This is a subtitle", after = 0),
                                 Subtotal(name = "B alone", categories = c("B"),
                                          after = 1),
                                 Subtotal(name = "C alone", categories = c(2),
                                          after = "C"),
                                 Subtotal(name = "B+C", categories = c(1, 2),
                                          after = 999))
        trans_resp <- trans
        trans_resp["categories"] <- list(NULL)
        trans_resp["elements"] <- list(NULL)

        expect_json_equivalent(transforms(ds$v4), trans_resp)

        # v4_ary <- array(c(10, 10, 20), dim = c(3, 1),
        #                 dimnames = list(c("B", "C", "B+C"), "Count"))
        # expect_equivalent(showTransforms(ds$v4), v4_ary)
    })
})
