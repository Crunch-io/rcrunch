context("Subtotals and headings ")

insrts_list <- list(list(anchor = 0, name = "First one",
                         `function` = "subtotal", args = c(3, 4)),
                    list(anchor = 999, name = "Last one",
                         `function` = "subtotal", args = c(5, 6)),
                    list(anchor = 10, name = "High",
                         `function` = "subtotal", args = c(9, 10)),
                    list(anchor = 2, name = "Low",
                         `function` = "subtotal", args = c(1, 2)),
                    list(anchor = 8, name = "missing anchor",
                         `function` = "subtotal", args = c(2, 3)),
                    list(anchor = 4, name = "missing categories",
                         `function` = "subtotal", args = c(7, 8)))
trans <- Transforms(insertions = Insertions(data=insrts_list))

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
        '{"anchor":4,"name":"Women","function":"subtotal","args":2},',
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
        '{"anchor":1,"name":"London alone","function":"subtotal","args":1},',
        '{"anchor":2,"name":"Scotland alone","function":"subtotal","args":2},',
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
        '{"anchor":1,"name":"London alone","function":"subtotal","args":1}]}}}}')

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