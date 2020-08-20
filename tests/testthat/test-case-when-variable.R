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
    ds <- newDataset(df)

    test_that("casewhen works for categorical variable", {
        ds$case_when_cat <- makeCaseWhenVariable(
            ds$v3 <= 10 ~ Category(name = "new cat", numeric_value = 5),
            ds$v3 <= 15 ~ ds$v4,
            TRUE ~ "else category",
            name = "case when categorical"
        )

        expect_equal(
            as.vector(ds$case_when_cat),
            factor(
                c(
                    "new cat", "new cat", "new cat",
                    "C", "B", "C", "B", "C",
                    rep("else category", 12)
                ),
                c("new cat", "else category", "B", "C")
            )
        )

        expect_equal(
            names(categories(ds$case_when_cat)),
            c("new cat", "else category", "No Data", "B", "C")
        )

        expect_equal(values(categories(ds$case_when_cat)["new cat"]), 5)

        expect_equal(name(ds$case_when_cat), "case when categorical")
    })

    test_that("casewhen works for numeric variable", {
        ds$case_when_num <- makeCaseWhenVariable(
            ds$v3 <= 10 ~ 10,
            ds$v4 == "B" ~ ds$v3,
            name = "case when numeric"
        )

        expect_equal(
            as.vector(ds$case_when_num),
            c(10, 10, 10, NA, 12, NA, 14, NA, 16, NA, 18, NA, 20, NA, 22, NA, 24, NA, 26, NA)
        )

        expect_equal(name(ds$case_when_num), "case when numeric")
    })
})