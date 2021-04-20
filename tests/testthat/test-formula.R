context("Formulas")


with_mock_crunch({
    ds <- cachedLoadDataset("test ds")

    test_that("parseTerms", {
        rhs_data <- parseTerms(~ gender + birthyr, data = ds)
        expect_equivalent(rhs_data, list(ds$gender, ds$birthyr))
        lhs_data <- parseTerms(birthyr ~ gender, data = ds, side = "LHS")
        expect_equivalent(lhs_data, list(ds$birthyr))
        lhs_data <- parseTerms(~gender, data = ds, side = "LHS")
        expect_equivalent(lhs_data, list())
        expect_error(
            parseTerms(~gender, data = ds, side = "not a side"),
            "unknown side specification for parsing formulae."
        )
        expect_error(
            parseTerms(~1, data = ds, side = "LHS"),
            "Must supply one or more variables"
        )
        expect_error(
            parseTerms(. ~ gender, data = ds, side = "LHS"),
            paste0("Crunch formulae do not support ", dQuote("."), " in formula")
        )
    })

    test_that("formulaToQuery can accept both data and calling env. references", {
        ftq_with_data <- formulaToQuery(~ gender + birthyr, data = ds)
        expect_is(ftq_with_data, "list")
        ftq_without_data <- formulaToQuery(~ ds$gender + ds$birthyr)
        expect_is(ftq_without_data, "list")
        expect_equal(ftq_with_data, ftq_without_data)
    })

    test_that("formulaToQuery expands dimensions for as_selected expressions", {
        expect_equal(
            formulaToQuery(~selectCategories(ds$catarray, 1)),
            list(
                dimensions = list(list(
                    list(
                        `function` = "dimension",
                        args = list(
                            unclass(zcl(selectCategories(ds$catarray, 1))),
                            unclass(zcl("subvariables"))
                        )
                    ),
                    unclass(zcl(selectCategories(ds$catarray, 1)))
                )),
                measures = list(count = list(`function` = "cube_count", args = list()))
            )
        )
    })

    test_that("registerCubeFunctions work within formulaToQuery - mean", {
        expect_equal(
            formulaToQuery(mean(ds$birthyr)~1),
            list(
                dimensions = list(),
                measures = list(
                    mean = list(
                        `function` = "cube_mean",
                        args = list(list(variable = self(ds$birthyr)))
                    )
                )
            )
        )
    })

    test_that("registerCubeFunctions work within formulaToQuery - as_selected", {
        expect_equal(
            formulaToQuery(~as_selected(ds$mymrset)),
            list(
                dimensions = list(list(
                    list(`function` = "dimension", args = list(
                         list(
                             `function` = "as_selected",
                             args = list(list(variable = self(ds$mymrset)))
                         ),
                         list(value = "subvariables")
                    )),
                    list(
                        `function` = "as_selected",
                        args = list(list(variable = self(ds$mymrset)))
                    )
                )),
                measures = list(count = list(`function` = "cube_count", args = list()))
            )
        )
    })

    test_that("registerCubeFunctions work within formulaToQuery - as_array", {
        expect_equal(
            formulaToQuery(~as_array(ds$mymrset)),
            list(
                dimensions = list(list(
                    list(
                        "function" = "dimension",
                        args = list(
                            list(variable = self(ds$mymrset)), list(value = "subvariables")
                        )
                    ),
                    list(variable = self(ds$mymrset))
                )),
                measures = list(count = list(`function` = "cube_count", args = list()))
            )
        )
    })

    test_that("registerCubeFunctions work within formulaToQuery - categories", {
        expect_equal(
            formulaToQuery(~categories(ds$catarray)),
            list(
                dimensions = list(list(
                    list(variable = self(ds$catarray))
                )),
                measures = list(count = list(`function` = "cube_count", args = list()))
            )
        )
    })

    test_that("registerCubeFunctions work within formulaToQuery - subvariables", {
        expect_equal(
            formulaToQuery(~subvariables(ds$catarray)),
            list(
                dimensions = list(list(
                    list(
                        `function` = "dimension",
                        args = list(
                            list(variable = self(ds$catarray)),
                            zcl("subvariables")
                        )
                    )
                )),
                measures = list(count = list(`function` = "cube_count", args = list()))
            )
        )
    })

    test_that("scorecard works", {
        expect_equal(
            formulaToQuery(~scorecard(ds$mymrset, selectCategories(ds$catarray, "A"))),
            list(
                measures = list(count = list(`function` = "cube_count", args = list())),
                with = list(
                    v1 = list(
                        `function` = "fuse",
                        args = list(
                            list(
                                list(
                                    `function` = "as_selected",
                                    args = list(list(variable = self(ds$mymrset)))
                                ),
                                list(
                                    `function` = "as_selected",
                                    args = list(
                                        list(
                                            `function` = "select_categories",
                                            args = list(
                                                list(variable = self(ds$catarray)),
                                                list(value = I("A"))
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                ),
                dimensions = list(list(
                    list(local = "v1.subvariables"),
                    list(local = "v1.categories"),
                    list(local = "v1.variables")
                ))
            )
        )

        expect_error(
            formulaToQuery(~scorecard(ds$catarray)),
            "Expected Multiple Response variables or expressions in a scorecard"
        )
        expect_error(
            formulaToQuery(~scorecard(ds$mymrset) + ds$gender),
            "scorecard queries cannot be combined with other dimensions"
        )
        expect_error(
            formulaToQuery(mean(ds$birthyr) ~ scorecard(ds$mymrset)),
            "scorecard queries can only have count as the aggregation"
        )
    })
})
