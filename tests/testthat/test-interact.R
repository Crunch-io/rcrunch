context("Interact")

with_mock_crunch({
    ds <- cachedLoadDataset("test ds")

    test_that("interactVariables", {
        interaction_var <- interactVariables(
            ds$gender, ds$gender,
            name = "interaction!",
            collapse_missings = TRUE
        )
        expect_is(interaction_var, "VariableDefinition")

        # Two arguments, first with the 2 variables, the second with collapse_missings
        expect_equal(length(interaction_var$derivation$args), 2)
        expect_equal(length(interaction_var$derivation$args[[1]]), 2)
        expect_equal(names(interaction_var$derivation$args[[1]][[1]]), "variable")
        expect_equal(names(interaction_var$derivation$args[[1]][[2]]), "variable")
        expect_equal(interaction_var$derivation$args[[2]], list(value = TRUE))
        expect_POST(
            ds$interaction <- interaction_var,
            "https://app.crunch.io/api/datasets/1/variables/"
        )
        expect_error(
            interactVariables(ds$gender, ds$gender),
            'argument "name" is missing, with no default'
        )
        expect_error(
            interactVariables(ds$gender, name = "one var"),
            "must supply more than one variable to make an interaction"
        )
        expect_error(
            interactVariables(name = "no vars"),
            "must supply more than one variable to make an interaction"
        )
        expect_warning(
            interaction_var <- interactVariables(
                ds$gender,
                ds$gender,
                name = "interaction!",
                sep = " - "
            ),
            "The `sep` argument is no longer supported"
        )
    })
})



with_test_authentication({
    ds <- newDatasetFromFixture("apidocs")
    test_that("makeInteractions", {
        ds$interaction <- interactVariables(ds$q1, ds$country, name = "Pet.Country")

        q1_cats <- names(categories(ds$q1))
        country_cats <- names(categories(ds$country))

        expect_equal(
            names(categories(ds$interaction)),
            c(
                paste(
                    rep(q1_cats, each = length(country_cats)),
                    rep(country_cats, times = length(q1_cats)),
                    sep = " and "
                ),
                "No Data"
            )
        )
    })

    test_that("makeInteractions accepts option arguments", {
        ds$interaction2 <- interactVariables(ds$q1, ds$country,
            name = "Pet.Country2",
            description = "This is a description",
            collapse_missings = TRUE
        )

        q1_cats_no_miss <- names(categories(ds$q1)[!is.na(categories(ds$q1))])
        country_cats_no_miss <- names(categories(ds$country)[!is.na(categories(ds$country))])

        expect_equal(
            names(categories(ds$interaction2)),
            c(
                paste(
                    rep(q1_cats_no_miss, each = length(country_cats_no_miss)),
                    rep(country_cats_no_miss, times = length(q1_cats_no_miss)),
                    sep = " and "
                ),
                "No Data"
            )
        )
        expect_equal(description(ds$interaction2), "This is a description")
    })
})
