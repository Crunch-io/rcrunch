context("fill variable")

with_mock_crunch({
    basic_fill_expr <- paste0(
        '{"function":"fill","args":[{"variable":"https://app.crunch.io/api/datasets/1/variables/gender/"},', #nolint
        '{"map":{"1":{"variable":"https://app.crunch.io/api/datasets/1/variables/location/"}}}]}'
    )

    expr_fill_expr <- paste0(
        '{"function":"fill","args":[{"function":"as_selected","args":',
        '[{"function":"select_categories","args":',
        '[{"variable":"https://app.crunch.io/api/datasets/1/variables/gender/"},',
        '{"value":["Male"]}]}]},{"map":',
        '{"1":{"variable":"https://app.crunch.io/api/datasets/1/variables/location/"}}}]}'
    )


    ds <- loadDataset("test ds")

    test_that("fillExpr works on existing variable with ids in list", {
        expect_equal(
            unclass(toJSON(
                fillExpr(ds$gender, list(list(id = 1, fill = ds$location)))@expression
            )),
            basic_fill_expr
        )
    })

    test_that("fillExpr works on existing variable with names in list", {
        expect_equal(
            unclass(toJSON(
                fillExpr(ds$gender, list(list(name = "Male", fill = ds$location)))@expression
            )),
            basic_fill_expr
        )
    })

    test_that("fillExpr works on existing variable with values in list", {
        expect_equal(
            unclass(toJSON(
                fillExpr(ds$gender, list(list(value = 1, fill = ds$location)))@expression
            )),
            basic_fill_expr
        )
    })

    test_that("fillExpr works on existing variable with names in named arguments", {
        expect_equal(
            unclass(toJSON(fillExpr(ds$gender, Male = ds$location)@expression)),
            basic_fill_expr
        )
    })

    test_that("fillExpr works on expression with ids in list", {
        expect_equal(
            unclass(toJSON(
                fillExpr(selectCategories(ds$gender, "Male"), list(list(id = 1, fill = ds$location)))@expression #nolint
            )),
            expr_fill_expr
        )
    })

    test_that("fillExpr works when with nse when passing data arg", {
        expect_equal(
            unclass(toJSON(
                fillExpr(gender, list(list(id = 1, fill = location)), data = ds)@expression
            )),
            basic_fill_expr
        )

        expect_equal(
            unclass(toJSON(
                fillExpr(gender, Male = location, data = ds)@expression
            )),
            basic_fill_expr
        )
    })

    test_that("fillExpr errors on bad inputs", {
        expect_error(fillExpr(ds$gender), "Must pass either")

        expect_error(fillExpr(
            ds$gender,
            fills = list(list(id = 1, fill = ds$location)),
            Female = ds$warm_drink
        ), "Cannot pass both")

        expect_error(
            fillExpr(ds$gender, list(list(id = 1))),
            "must have a fill"
        )

        expect_error(
            fillExpr(ds$gender, list(list(fill = ds$location))),
            "All fills must have a category id"
        )

        expect_error(
            fillExpr(ds$gender, list(list(id = 3, fill = ds$location))),
            "id '3' does not uniquely identify a "
        )

        expect_error(
            fillExpr(
                selectCategories(ds$gender, "Male"),
                list(list(value = 1, fill = ds$location))
            ),
            "must specify id when categories are not available"
        )
    })
})
