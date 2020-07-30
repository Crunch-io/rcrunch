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
                ',{"map":{"1":{"variable":"https://app.crunch.io/api/datasets/1/variables/gender/"',
                '}}}]}'
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

    test_that("caseWhenExpr handles formulas argument", {
        expect_equal(
            caseWhenExpr(
                crunchBetween(ds$birthyr, 1970, 1980) ~ Category(name = "Hello"),
                crunchBetween(ds$birthyr, 1980, 1990) ~ ds$gender,
                TRUE ~ Category(name = "Missed Q", missing = TRUE)
            ),
            caseWhenExpr(
                formulas = list(
                    crunchBetween(ds$birthyr, 1970, 1980) ~ Category(name = "Hello"),
                    crunchBetween(ds$birthyr, 1980, 1990) ~ ds$gender,
                    TRUE ~ Category(name = "Missed Q", missing = TRUE)
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
            "The left-hand side provided must be a logical or a CrunchLogicalExpr: \"ds.gender\""
        )

        expect_error(
            makeCaseWhenVariable(ds$birthyr > 1980 ~ 1),
            paste0(
                "The right-hand side provided must be a Category, CrunchVariable ",
                "string, or `NA`: \"1\""
            )
        )
    })
})