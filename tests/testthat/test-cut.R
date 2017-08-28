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
        expect_identical( generateCutLabels(2, c(2, 3, 4, 5), 4, FALSE, FALSE),
            c("[2,3)", "[3,4)", "[4,5)")
        )
    })

    test_that("cut throws error when no variable name supplies", {
        expect_error( cut(ds$birthyr, 3), "Must provide the name for the new variable")
    })


    test_that("cut returns expected output", {

        ##################### EXPECTED OUTPUT ######################################
        basic_output <- list(
            name = "new_var",
            derivation = list(
                `function` = "case",
                args = list(
                    list(
                        column = I(1:3),
                        type = list(
                            value = list(
                                class = "categorical",
                                categories = list(
                                    list(id = 1L,
                                        name = "one",
                                        numeric_value = NULL,
                                        missing = FALSE),
                                    list(id = 2L,
                                        name = "two",
                                        numeric_value = NULL,
                                        missing = FALSE),
                                    list(id = 3L,
                                        name = "three",
                                        numeric_value = NULL,
                                        missing = FALSE))))),
                    list(
                        `function` = "and",
                        args = list(
                            list(
                                `function` = ">",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"),
                                    list(value = -1.4998629))),
                            list(`function` = "<=",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"),
                                    list(value = -0.4424))))),
                    list(`function` = "and",
                        args = list(
                            list(
                                `function` = ">",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"),
                                    list(value = -0.4424))),
                            list(`function` = "<=",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"),
                                    list(value = 0.6119)
                                )
                            )
                        )
                    ),
                    list(`function` = "and",
                        args = list(
                            list(`function` = ">",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"),
                                    list(value = 0.6119))),
                            list(`function` = "<=",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"),
                                    list(value = 1.6693629)
                                )
                            )
                        )
                    )
                )
            )
        )


        right_false <- list(
            name = "new_var",
            derivation = list(
                `function` = "case",
                args = list(
                    list(column = I(1:3),
                        type = list(
                            value = list(
                                class = "categorical",
                                categories = list(
                                    list(id = 1L,
                                        name = "one",
                                        numeric_value = NULL,
                                        missing = FALSE),
                                    list(id = 2L,
                                        name = "two",
                                        numeric_value = NULL,
                                        missing = FALSE),
                                    list(id = 3L,
                                        name = "three",
                                        numeric_value = NULL,
                                        missing = FALSE)
                                )
                            )
                        )
                    ),
                    list(`function` = "and",
                        args = list(
                            list(
                                `function` = ">=",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"),
                                    list(value = -1.4998629))), list(`function` = "<",
                                        args = list(
                                            list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"),
                                            list(value = -0.4424)
                                        )
                                    )
                        )
                    ),
                    list(`function` = "and",
                        args = list(
                            list(`function` = ">=",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"),
                                    list(value = -0.4424)
                                )
                            ),
                            list(`function` = "<",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"),
                                    list(value = 0.6119)
                                )
                            )
                        )
                    ),
                    list(`function` = "and",
                        args = list(
                            list(`function` = ">=",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"),
                                    list(value = 0.6119)
                                )
                            ), list(`function` = "<",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"),
                                    list(value = 1.6693629)
                                )
                            )
                        )
                    )
                )
            )
        )

        set_breaks <- list(
            name = "new_var",
            derivation = list(
                `function` = "case",
                args = list(
                    list(
                        column = I(1:2),
                        type = list(
                            value = list(class = "categorical",
                                categories = list(
                                    list(id = 1L,
                                        name = "one",
                                        numeric_value = NULL,
                                        missing = FALSE),
                                    list(id = 2L,
                                        name = "two",
                                        numeric_value = NULL,
                                        missing = FALSE)
                                )
                            )
                        )
                    ),
                    list(`function` = "and",
                        args = list(
                            list(`function` = ">=",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"),
                                    list(value = -1.4967)
                                )
                            ),
                            list(`function` = "<",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"),
                                    list(value = 0)
                                )
                            )
                        )
                    ),
                    list(`function` = "and",
                        args = list(
                            list(`function` = ">=",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"),
                                    list(value = 0)
                                )
                            ),
                            list(`function` = "<",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"),
                                    list(value = 1.6662)
                                )
                            )
                        )
                    )
                )
            )
        )

        dig_lab <- list(name = "new_var",
            derivation = list(
                `function` = "case",
                args = list(
                    list(
                        column = I(1:3),
                        type = list(
                            value = list(
                                class = "categorical",
                                categories = list(
                                    list(id = 1L,
                                        name = "(-1.5,-0.44]",
                                        numeric_value = NULL,
                                        missing = FALSE),
                                    list(id = 2L,
                                        name = "(-0.44,0.61]",
                                        numeric_value = NULL,
                                        missing = FALSE),
                                    list(id = 3L,
                                        name = "(0.61,1.7]",
                                        numeric_value = NULL,
                                        missing = FALSE)
                                )
                            )
                        )
                    ),
                    list(
                        `function` = "and",
                        args = list(
                            list(`function` = ">",
                                args = list(list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"),
                                    list(value = -1.4998629)
                                )
                            ),
                            list(`function` = "<=",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"),
                                    list(value = -0.4424)
                                )
                            )
                        )
                    ),
                    list(
                        `function` = "and",
                        args = list(list(`function` = ">",
                            args = list(
                                list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"),
                                list(value = -0.4424)
                            )
                        ),
                            list(`function` = "<=",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"),
                                    list(value = 0.6119)
                                )
                            )
                        )
                    ),
                    list(`function` = "and",
                        args = list(
                            list(
                                `function` = ">",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"),
                                    list(value = 0.6119)
                                )
                            ),
                            list(`function` = "<=",
                                args = list(
                                    list(variable = "https://app.crunch.io/api/datasets/1/variables/birthyr/"),
                                    list(value = 1.6693629)
                                )
                            )
                        )
                    )
                )
            )
        )


        ##############################################################################
        basic_cut <- cut(ds$birthyr, 3, name = "new_var", label = c("one", "two", "three"))
        expect_is(
            basic_cut,
            "VariableDefinition"
        )

        expect_equivalent(
            basic_output,
            basic_cut
        )

        expect_equivalent(
            right_false,
            cut(ds$birthyr,
                3,
                name = "new_var",
                label = c("one", "two", "three"),
                right = FALSE)
        )

        expect_equivalent(
            set_breaks,
            cut(ds$birthyr,
                c(-1.4967, 0, 1.6662),
                name = "new_var",
                label = c("one", "two"),
                right = FALSE)
        )
        expect_equivalent(
            dig_lab,
            cut(ds$birthyr,
                3,
                dig.lab = 2,
                name = "new_var")
        )
    })
})

with_test_authentication({
    ds <- newDataset(df)

    test_that("cut returns the same thing for crunch variables and identical vectors", {
        ds$cat_var1 <- cut(ds$v1, 3, name = "new_var1", label = c("one", "two", "three"))
        expect_identical(
            cut(df$v1, 3, label = c("one", "two", "three")),
            as.vector(ds$cat_var1)
        )

        ds$cat_var2 <- cut(ds$v1, 3, name = "new_var2", label = c("one", "two", "three"),
            right = FALSE)
        expect_identical(
            cut(df$v1, 3, label = c("one", "two", "three"), right = FALSE),
            as.vector(ds$cat_var2)
        )

        ds$cat_var3 <- cut(ds$v1,
            c(-1.4967, 0, 1.6662),
            name = "new_var3",
            label = c("one", "two"),
            right = FALSE)
        expect_identical(
            cut(df$v1, c(-1.4967, 0, 1.6662), label = c("one", "two"), right = FALSE),
            as.vector(ds$cat_var3)
        )

        ds$cat_var4 <- cut(ds$v1,
            3,
            dig.lab = 2,
            name = "new_var4")
        expect_identical(
            cut(df$v1, 3, dig.lab = 2),
            as.vector(ds$cat_var4)
        )

        ds$cat_var5 <- cut(ds$v1,
            3,
            include.lowest = TRUE,
            name = "new_var5")
        expect_identical(
            cut(df$v1, 3, include.lowest = TRUE),
            as.vector(ds$cat_var5)
        )
    })
})
