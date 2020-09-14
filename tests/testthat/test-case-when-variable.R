context("caseWhen variable")

with_mock_crunch({
    ds <- loadDataset("test ds")

    test_that("caseWhenExpr works when single rhs variable", {
        expect_equal(
            unclass(toJSON(
                caseWhenExpr(ds$birthyr > 1970 ~ ds$gender)@expression
            )),
            paste0(
                '{"function":"fill","args":[{"function":"case","args":[{"column":[1],"type":{',
                '"value":{"class":"categorical","categories":[',
                '{"id":1,"name":"casefill__internal1","numeric_value":null,"missing":false}',
                ']}}},{"function":">","args":[{"variable"',
                ':"https://app.crunch.io/api/datasets/1/variables/birthyr/"},{"value":1970}]}]}',
                ',{"map":{"1":{"variable":"https://app.crunch.io/api/datasets/1/variables/gender/"}}}]}' # nolint
            )
        )
    })

    test_that("caseWhenExpr works when single rhs Category", {
        expect_equal(
            unclass(toJSON(
                caseWhenExpr(ds$birthyr > 1970 ~ Category(name = "Hello"))@expression
            )),
            paste0(
                '{"function":"case","args":[{"column":[1],"type":{"value":{"class":"categorical",',
                '"categories":[{"id":1,"name":"Hello","numeric_value":null,"missing":false}]}}},',
                '{"function":">","args":[{"variable":',
                '"https://app.crunch.io/api/datasets/1/variables/birthyr/"},{"value":1970}]}]}'
            )
        )
    })

    test_that("caseWhenExpr works when variable + rhs string + else statement", {
        expect_equal(
            unclass(toJSON(
                caseWhenExpr(
                    crunchBetween(ds$birthyr, 1970, 1980) ~ Category(name = "Hello"),
                    crunchBetween(ds$birthyr, 1980, 1990) ~ ds$gender,
                    TRUE ~ Category(name = "Missed Q", missing = TRUE)
                )@expression
            )),
            paste0(
                '{"function":"fill","args":[{"function":"case","args":[{"column":[1,2,3],',
                '"type":{"value":{"class":"categorical","categories":[{"id":1,"name":"Hello",',
                '"numeric_value":null,"missing":false},{"id":2,"name":"casefill__internal2",',
                '"numeric_value":null,"missing":false},{"id":3,"name":"Missed Q",',
                '"numeric_value":null,"missing":true}]}}},{"function":"between","args"',
                ':[{"variable":"https://app.crunch.io/api/datasets/1/variables/birthyr/"},',
                '{"value":1970},{"value":1980},{"value":[true,false]}]},{"function":"between",',
                '"args":[{"variable":"https://app.crunch.io/api/datasets/1/variables/birthyr/"},',
                '{"value":1980},{"value":1990},{"value":[true,false]}]}]},{"map":{"2":{"variable":',
                '"https://app.crunch.io/api/datasets/1/variables/gender/"}}}]}'
            )
        )
    })

    test_that("caseWhenExpr works with numbers in rhs", {
        expect_equal(
            unclass(toJSON(
                caseWhenExpr(ds$birthyr < 1970 ~ 1970)@expression
            )),
            paste0(
                '{"function":"numeric_fill","args":[{"function":"case","args":[{"column":[1],"type":{', #nolint
                '"value":{"class":"categorical","categories":[',
                '{"id":1,"name":"casefill__internal1","numeric_value":null,"missing":false}',
                ']}}},{"function":"<","args":[{"variable"',
                ':"https://app.crunch.io/api/datasets/1/variables/birthyr/"},{"value":1970}]}]}',
                ',{"map":{"1":{"value":1970,"type":"numeric"}}}]}'
            )
        )
    })

    test_that("caseWhenExpr handles formulas in cases argument", {
        expect_equal(
            caseWhenExpr(
                crunchBetween(ds$birthyr, 1970, 1980) ~ Category(name = "Hello"),
                crunchBetween(ds$birthyr, 1980, 1990) ~ ds$gender,
                TRUE ~ Category(name = "Missed Q", missing = TRUE)
            ),
            caseWhenExpr(
                cases = list(
                    crunchBetween(ds$birthyr, 1970, 1980) ~ Category(name = "Hello"),
                    crunchBetween(ds$birthyr, 1980, 1990) ~ ds$gender,
                    TRUE ~ Category(name = "Missed Q", missing = TRUE)
                )
            )
        )
    })

    test_that("caseWhenExpr handles lists in cases argument", {
        expect_equal(
            caseWhenExpr(
                crunchBetween(ds$birthyr, 1970, 1980) ~ Category(name = "Hello"),
                crunchBetween(ds$birthyr, 1980, 1990) ~ ds$gender,
                TRUE ~ Category(name = "Missed Q", missing = TRUE)
            ),
            caseWhenExpr(
                cases = list(
                    list(
                        expression = crunchBetween(ds$birthyr, 1970, 1980),
                        name = "Hello"
                    ),
                    list(
                        expression = crunchBetween(ds$birthyr, 1980, 1990),
                        fill = ds$gender
                    ),
                    list(
                        expression = TRUE,
                        name = "Missed Q",
                        missing = TRUE
                    )
                )
            )
        )
    })

    test_that("caseWhenExpr handles data argument", {
        expect_equal(
            caseWhenExpr(
                crunchBetween(birthyr, 1970, 1980) ~ Category(name = "Hello"),
                crunchBetween(birthyr, 1980, 1990) ~ gender,
                TRUE ~ Category(name = "Missed Q", missing = TRUE),
                data = ds
            ),
            caseWhenExpr(
                crunchBetween(ds$birthyr, 1970, 1980) ~ Category(name = "Hello"),
                crunchBetween(ds$birthyr, 1980, 1990) ~ ds$gender,
                TRUE ~ Category(name = "Missed Q", missing = TRUE)
            )
        )
    })

    test_that("makeCaseWhenVariable handles data argument", {
        expect_equal(
            makeCaseWhenVariable(
                crunchBetween(birthyr, 1970, 1980) ~ Category(name = "Hello"),
                crunchBetween(birthyr, 1980, 1990) ~ gender,
                TRUE ~ Category(name = "Missed Q", missing = TRUE),
                data = ds,
                name = "test"
            ),
            makeCaseWhenVariable(
                crunchBetween(ds$birthyr, 1970, 1980) ~ Category(name = "Hello"),
                crunchBetween(ds$birthyr, 1980, 1990) ~ ds$gender,
                TRUE ~ Category(name = "Missed Q", missing = TRUE),
                name = "test"
            )
        )
    })

    test_that("makeCaseWhenVariable correctly separates dots", {
        expect_equal(
            makeCaseWhenVariable(
                crunchBetween(ds$birthyr, 1970, 1980) ~ Category(name = "Hello"),
                crunchBetween(ds$birthyr, 1980, 1990) ~ ds$gender,
                TRUE ~ Category(name = "Missed Q", missing = TRUE),
                name = "test",
                description = "desc"
            ),
            VarDef(
                caseWhenExpr(
                    crunchBetween(ds$birthyr, 1970, 1980) ~ Category(name = "Hello"),
                    crunchBetween(ds$birthyr, 1980, 1990) ~ ds$gender,
                    TRUE ~ Category(name = "Missed Q", missing = TRUE)
                ),
                name = "test",
                description = "desc"
            )
        )
    })


    test_that("caseWhenExpr formula validations", {
        expect_error(
            makeCaseWhenVariable(~ds$gender),
            "The condition provided must be a proper formula: .ds.gender"
        )

        expect_error(
            makeCaseWhenVariable(ds$gender ~ ds$gender),
            "The left-hand side provided must be a logical or a CrunchLogicalExpr:"
        )

        expect_error(
            makeCaseWhenVariable(ds$birthyr > 1980 ~ list(x = 1)),
            paste0(
                "The right-hand side provided must be a Category, CrunchVariable ",
                "string, number, or `NA`:"
            )
        )
    })
})

with_test_authentication({
    ds <- newDataset(
        data.frame(
            fav_brand1 = factor(
                c("Coke", "Diet Coke", "Diet Pepsi", "Coke", "Pepsi", "Water"),
                c("Coke", "Diet Coke", "Pepsi", "Diet Pepsi", "Water")
            ),
            fav_brand2 = factor(
                c("Diet Coke", "Pepsi", "Coke", "Diet Coke", "Diet Pepsi", "Pepsi"),
                c("Coke", "Diet Coke", "Pepsi", "Diet Pepsi", "Water")
            ),
            rating1 = c(9, 9, 7, 9, 8, 10),
            rating2 = c(7, 2, 7, 8, 6, 3),
            stringsAsFactors = FALSE
        )
    )

    test_that("casewhen works for categorical variable", {
        ds$coke_rival <- makeCaseWhenVariable(
            ds$fav_brand1 %in% c("Coke", "Diet Coke") &
                ds$fav_brand2 %in% c("Coke", "Diet Coke") ~ "Coke loyal",
            ds$fav_brand1 %in% c("Coke", "Diet Coke") ~ ds$fav_brand2,
            ds$fav_brand2 %in% c("Coke", "Diet Coke") ~ ds$fav_brand1,
            TRUE ~ Category(name = "Never interested", missing = TRUE),
            name = "Rival soda for those with Coke products in top 2"
        )

        expect_equal(
            as.vector(ds$coke_rival),
            factor(
                c("Coke loyal", "Pepsi", "Diet Pepsi", "Coke loyal", NA, NA),
                c("Coke loyal", "Coke", "Diet Coke", "Pepsi", "Diet Pepsi", "Water")
            )
        )

        expect_equal(
            names(categories(ds$coke_rival)),
            c("Coke loyal", "Never interested", "No Data", "Coke", "Diet Coke", "Pepsi", "Diet Pepsi", "Water") #nolint
        )

        expect_equal(name(ds$coke_rival), "Rival soda for those with Coke products in top 2")
    })

    test_that("casewhen works for numeric variable", {
        ds$coke_score <- makeCaseWhenVariable(
            ds$fav_brand1 == "Coke" ~ ds$rating1,
            ds$fav_brand2 == "Coke" ~ ds$rating2,
            ds$fav_brand1 == "Diet Coke" | ds$fav_brand2 == "Diet Coke" ~ 5,
            name = "Coke score"
        )

        expect_equal(
            as.vector(ds$coke_score),
            c(9, 5, 7, 9, NA, NA)
        )

        expect_equal(name(ds$coke_score), "Coke score")
    })
})