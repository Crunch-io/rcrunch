context("cut function")

with_mock_crunch({
    ds <- loadDataset("test ds")
    gen <- ds$gender

    test_that("cut labels are generated correcty", {
        expect_identical(
            generateCutLabels(5, c(2.111111, 3, 4, 5), 4, FALSE, FALSE),
            c("[2.1111,3)", "[3,4)", "[4,5)")
        )
        expect_identical(
            generateCutLabels(2, c(2.111111, 3, 4, 5), 4, FALSE, FALSE),
            c("[2.1,3)", "[3,4)", "[4,5)")
        )
        expect_identical(
            generateCutLabels(2, c(2, 3, 4, 5), 4, FALSE, FALSE),
            c("[2,3)", "[3,4)", "[4,5)")
        )
        expect_identical(
            generateCutLabels(5, c(2.111111, 3, 4, 5), 4, TRUE, TRUE),
            c("[2.1111,3]", "(3,4]", "(4,5]")
        )
        expect_identical(
            generateCutLabels(5, c(2.111111, 3, 4, 5), 4, FALSE, TRUE),
            c("[2.1111,3)", "[3,4)", "[4,5]")
        )
        # if the breaks are all the same at up to 12 decimal points
        # generateCutLabels will use Range_n lagles.
        expect_identical(
            generateCutLabels(
                5,
                c(
                    2.111111111111, 2.111111111112,
                    2.111111111113, 2.111111111114
                ),
                4, FALSE, FALSE
            ),
            c("Range_1", "Range_2", "Range_3")
        )
    })

    test_that("cut throws error when no variable name supplies", {
        expect_error(
            cut(ds$birthyr, 3),
            "Must provide the name for the new variable"
        )
    })


    test_that("cut returns expected output", {

        ##################### EXPECTED OUTPUT ######################################
        basic_output <- list(
            derivation = list(
                `function` = "case",
                args = list(
                    list(
                        column = I(1:3),
                        type = list(
                            value = list(
                                class = "categorical",
                                categories = list(
                                    list(
                                        id = 1L,
                                        name = "one",
                                        numeric_value = NULL,
                                        missing = FALSE
                                    ),
                                    list(
                                        id = 2L,
                                        name = "two",
                                        numeric_value = NULL,
                                        missing = FALSE
                                    ),
                                    list(
                                        id = 3L,
                                        name = "three",
                                        numeric_value = NULL,
                                        missing = FALSE
                                    )
                                )
                            )
                        )
                    ),
                    list(
                        `function` = "and",
                        args = list(
                            list(
                                `function` = ">",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), # nolint
                                    structure(list(value = -1.4998629), class = "zcl")
                                )
                            ),
                            list(
                                `function` = "<=",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), # nolint
                                    structure(list(value = -0.4424), class = "zcl")
                                )
                            )
                        )
                    ),
                    list(
                        `function` = "and",
                        args = list(
                            list(
                                `function` = ">",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), # nolint
                                    structure(list(value = -0.4424), class = "zcl")
                                )
                            ),
                            list(
                                `function` = "<=",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), # nolint
                                    structure(list(value = 0.6119), class = "zcl")
                                )
                            )
                        )
                    ),
                    list(
                        `function` = "and",
                        args = list(
                            list(
                                `function` = ">",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), # nolint
                                    structure(list(value = 0.6119), class = "zcl")
                                )
                            ),
                            list(
                                `function` = "<=",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), # nolint
                                    structure(list(value = 1.6693629), class = "zcl")
                                )
                            )
                        )
                    )
                )
            ),
            name = "new_var"
        )


        right_false <- list(
            derivation = list(
                `function` = "case",
                args = list(
                    list(
                        column = I(1:3),
                        type = list(
                            value = list(
                                class = "categorical",
                                categories = list(
                                    list(
                                        id = 1L,
                                        name = "one",
                                        numeric_value = NULL,
                                        missing = FALSE
                                    ),
                                    list(
                                        id = 2L,
                                        name = "two",
                                        numeric_value = NULL,
                                        missing = FALSE
                                    ),
                                    list(
                                        id = 3L,
                                        name = "three",
                                        numeric_value = NULL,
                                        missing = FALSE
                                    )
                                )
                            )
                        )
                    ),
                    list(
                        `function` = "and",
                        args = list(
                            list(
                                `function` = ">=",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), # nolint
                                    structure(list(value = -1.4998629), class = "zcl")
                                )
                            ), list(
                                `function` = "<",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), # nolint
                                    structure(list(value = -0.4424), class = "zcl")
                                )
                            )
                        )
                    ),
                    list(
                        `function` = "and",
                        args = list(
                            list(
                                `function` = ">=",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), # nolint
                                    structure(list(value = -0.4424), class = "zcl")
                                )
                            ),
                            list(
                                `function` = "<",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), # nolint
                                    structure(list(value = 0.6119), class = "zcl")
                                )
                            )
                        )
                    ),
                    list(
                        `function` = "and",
                        args = list(
                            list(
                                `function` = ">=",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), # nolint
                                    structure(list(value = 0.6119), class = "zcl")
                                )
                            ), list(
                                `function` = "<",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), # nolint
                                    structure(list(value = 1.6693629), class = "zcl")
                                )
                            )
                        )
                    )
                )
            ),
            name = "new_var"
        )

        set_breaks <- list(
            derivation = list(
                `function` = "case",
                args = list(
                    list(
                        column = I(1:2),
                        type = list(
                            value = list(
                                class = "categorical",
                                categories = list(
                                    list(
                                        id = 1L,
                                        name = "one",
                                        numeric_value = NULL,
                                        missing = FALSE
                                    ),
                                    list(
                                        id = 2L,
                                        name = "two",
                                        numeric_value = NULL,
                                        missing = FALSE
                                    )
                                )
                            )
                        )
                    ),
                    list(
                        `function` = "and",
                        args = list(
                            list(
                                `function` = ">=",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), # nolint
                                    structure(list(value = -1.4967), class = "zcl")
                                )
                            ),
                            list(
                                `function` = "<",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), # nolint
                                    structure(list(value = 0), class = "zcl")
                                )
                            )
                        )
                    ),
                    list(
                        `function` = "and",
                        args = list(
                            list(
                                `function` = ">=",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), # nolint
                                    structure(list(value = 0), class = "zcl")
                                )
                            ),
                            list(
                                `function` = "<",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), # nolint
                                    structure(list(value = 1.6662), class = "zcl")
                                )
                            )
                        )
                    )
                )
            ),
            name = "new_var"
        )

        dig_lab <- list(
            derivation = list(
                `function` = "case",
                args = list(
                    list(
                        column = I(1:3),
                        type = list(
                            value = list(
                                class = "categorical",
                                categories = list(
                                    list(
                                        id = 1L,
                                        name = "(-1.5,-0.44]",
                                        numeric_value = NULL,
                                        missing = FALSE
                                    ),
                                    list(
                                        id = 2L,
                                        name = "(-0.44,0.61]",
                                        numeric_value = NULL,
                                        missing = FALSE
                                    ),
                                    list(
                                        id = 3L,
                                        name = "(0.61,1.7]",
                                        numeric_value = NULL,
                                        missing = FALSE
                                    )
                                )
                            )
                        )
                    ),
                    list(
                        `function` = "and",
                        args = list(
                            list(
                                `function` = ">",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), # nolint
                                    structure(list(value = -1.4998629), class = "zcl")
                                )
                            ),
                            list(
                                `function` = "<=",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), # nolint
                                    structure(list(value = -0.4424), class = "zcl")
                                )
                            )
                        )
                    ),
                    list(
                        `function` = "and",
                        args = list(
                            list(
                                `function` = ">",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), # nolint
                                    structure(list(value = -0.4424), class = "zcl")
                                )
                            ),
                            list(
                                `function` = "<=",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), # nolint
                                    structure(list(value = 0.6119), class = "zcl")
                                )
                            )
                        )
                    ),
                    list(
                        `function` = "and",
                        args = list(
                            list(
                                `function` = ">",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), # nolint
                                    structure(list(value = 0.6119), class = "zcl")
                                )
                            ),
                            list(
                                `function` = "<=",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"), # nolint
                                    structure(list(value = 1.6693629), class = "zcl")
                                )
                            )
                        )
                    )
                )
            ),
            name = "new_var"
        )


        ########################################################################
        basic_cut <- cut(ds$birthyr, 3,
            name = "new_var",
            label = c("one", "two", "three")
        )
        expect_is(basic_cut, "VariableDefinition")
        expect_equivalent(basic_cut, basic_output)
        expect_equivalent(
            right_false,
            cut(ds$birthyr,
                3,
                name = "new_var",
                label = c("one", "two", "three"),
                right = FALSE
            )
        )

        expect_equivalent(
            set_breaks,
            cut(ds$birthyr,
                c(-1.4967, 0, 1.6662),
                name = "new_var",
                label = c("one", "two"),
                right = FALSE
            )
        )
        expect_equivalent(
            dig_lab,
            cut(ds$birthyr,
                3,
                dig.lab = 2,
                name = "new_var"
            )
        )
    })

    test_that("cut input validation", {
        expect_error(
            cut(ds$birthyr,
                breaks = c(-1.4967),
                name = "new_var",
                right = TRUE
            ),
            "invalid number of breaks"
        )

        expect_error(
            cut(ds$birthyr,
                breaks = c(-1.4967, 0, 1.6662, 3),
                name = "new_var",
                label = c("one", "two"),
                right = TRUE
            ),
            paste0(
                "There are 3 resulting categories but you only supplied 2 ",
                "labels. Change number of breaks or the number of labels."
            )
        )
        expect_error(
            cut(ds$birthyr,
                breaks = c(1, 1, 1),
                name = "new_var",
                label = c("one", "two"),
                right = TRUE
            ),
            paste0(sQuote("breaks"), " must be unique")
        )
    })
})

with_test_authentication({
    ds <- newDataset(df)

    test_that("cut returns the same thing for Crunch variables and identical vectors", {
        ds$cat_var1 <- cut(ds$v1, 3,
            name = "new_var1",
            label = c("one", "two", "three")
        )
        expect_identical(
            cut(df$v1, 3, label = c("one", "two", "three")),
            as.vector(ds$cat_var1)
        )

        ds$cat_var2 <- cut(ds$v1, 3,
            name = "new_var2",
            label = c("one", "two", "three"), right = FALSE
        )
        expect_identical(
            cut(df$v1, 3, label = c("one", "two", "three"), right = FALSE),
            as.vector(ds$cat_var2)
        )

        ds$cat_var3 <- cut(ds$v1,
            c(-1.4967, 0, 1.6662),
            name = "new_var3",
            label = c("one", "two"),
            right = FALSE
        )
        expect_identical(
            cut(df$v1, c(-1.4967, 0, 1.6662), label = c("one", "two"), right = FALSE),
            as.vector(ds$cat_var3)
        )

        ds$cat_var4 <- cut(ds$v1,
            3,
            dig.lab = 2,
            name = "new_var4"
        )
        expect_identical(
            cut(df$v1, 3, dig.lab = 2),
            as.vector(ds$cat_var4)
        )

        ds$cat_var5 <- cut(ds$v1,
            3,
            include.lowest = TRUE,
            name = "new_var5"
        )
        expect_identical(
            cut(df$v1, 3, include.lowest = TRUE),
            as.vector(ds$cat_var5)
        )

        df$age <- sample(1:100, 20)
        ds$age <- df$age
        ds$age4 <- cut(
            df$age, c(0, 30, 45, 65, 200),
            c("youth", "adult", "middle-aged", "elderly")
        )
        expect_identical(
            cut(
                df$age, c(0, 30, 45, 65, 200),
                c("youth", "adult", "middle-aged", "elderly")
            ),
            as.vector(ds$age4)
        )
    })
})
