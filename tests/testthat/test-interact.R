context("Interact")

with_mock_crunch({
    ds <- loadDataset("test ds")
    test_that("makeInteractions", {
        test_cases <- makeInteractions(ds$gender, ds$mymrset[[1]], sep = ":")
        expect_equal(
            length(test_cases),
            length(categories(ds$gender)) * length(categories(ds$mymrset[[1]]))
        )
        expect_json_equivalent(
            test_cases[[1]],
            list(
                expression = ds$gender == "Male" & ds$mymrset[[1]] == "0.0",
                name = "Male:0.0",
                missing = FALSE
            )
        )
        expect_json_equivalent(
            test_cases[[3]],
            list(
                expression = ds$gender == "No Data" & ds$mymrset[[1]] == "0.0",
                name = "No Data:0.0",
                missing = TRUE
            )
        )
        test_cases <- makeInteractions(ds$gender, ds$mymrset[[1]], sep = ".")
        expect_json_equivalent(
            test_cases[[1]],
            list(
                expression = ds$gender == "Male" & ds$mymrset[[1]] == "0.0",
                name = "Male.0.0",
                missing = FALSE
            )
        )
        expect_error(
            makeInteractions(ds$gender, ds$textVar, sep = ":"),
            "makeInteractions can only take categorical variables"
        )
    })

    test_that("interactVariables", {
        interaction_var <- interactVariables(ds$gender, ds$gender, name = "interaction!")
        expect_is(interaction_var, "VariableDefinition")
        # There are 9 cases, plus variable definition
        expect_equal(length(interaction_var$derivation$arg), 10)
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
    })
})

with_test_authentication({
    ds <- newDatasetFromFixture("apidocs")
    test_that("makeInteractions", {
        test_cases <- makeInteractions(ds$q1, ds$country, sep = ":")
        expect_equal(
            length(test_cases),
            length(categories(ds$q1)) * length(categories(ds$country))
        )
        expect_json_equivalent(
            test_cases[[1]],
            list(
                expression = ds$q1 == "Cat" & ds$country == "Argentina",
                name = "Cat:Argentina",
                missing = FALSE
            )
        )
        tryCatch(
            expect_json_equivalent(
                test_cases[[11]],
                list(
                    expression = ds$q1 == "Not Asked" & ds$country == "Australia",
                    name = "Not Asked:Australia",
                    missing = TRUE
                )
            ),
            error = function(e) expect_json_equivalent(
                test_cases[[10]],
                list(
                    expression = ds$q1 == "Not Asked" & ds$country == "Australia",
                    name = "Not Asked:Australia",
                    missing = TRUE
                )
            )
        )

        ds$interaction <- interactVariables(ds$q1, ds$country, name = "Pet.Country")
        # Replace this `expect_true(isTRUE(all.equal(new)) || isTRUE(all.equal(old)))`
        # construction with `expect_equal(new)` once the "default values"
        # ticket https://www.pivotaltracker.com/story/show/164939686 is released.
        expect_true(
            isTRUE(all.equal(
                names(categories(ds$interaction)),
                c(
                    apply(
                        expand.grid(
                            c("Cat", "Dog", "Bird", "Skipped", "Not Asked", "No Data"),
                            c("Argentina", "Australia", "Austria", "Belgium", "Brazil", "No Data")
                        ),
                        1, paste,
                        collapse = ":"
                    ),
                    "No Data"
                )
            ))
            # Legacy output, if "No Data" categories are not automatically added:
            || isTRUE(all.equal(
                names(categories(ds$interaction)),
                apply(
                    expand.grid(
                        c("Cat", "Dog", "Bird", "Skipped", "Not Asked"),
                        c("Argentina", "Australia", "Austria", "Belgium", "Brazil")
                    ),
                    1, paste,
                    collapse = ":"
                )
            ))
        )
    })

    test_that("makeInteractions accepts opeion arguments", {
        ds$interaction2 <- interactVariables(ds$q1, ds$country,
            name = "Pet.Country2",
            description = "This is a description"
        )
        # Replace this `expect_true(isTRUE(all.equal(new)) || isTRUE(all.equal(old)))`
        # construction with `expect_equal(new)` once the "default values"
        # ticket https://www.pivotaltracker.com/story/show/164939686 is released.
        expect_true(
            isTRUE(all.equal(
                names(categories(ds$interaction)),
                c(
                    apply(
                        expand.grid(
                            c("Cat", "Dog", "Bird", "Skipped", "Not Asked", "No Data"),
                            c("Argentina", "Australia", "Austria", "Belgium", "Brazil", "No Data")
                        ),
                        1, paste,
                        collapse = ":"
                    ),
                    "No Data"
                )
            ))
            # Legacy output, if "No Data" categories are not automatically added:
            || isTRUE(all.equal(
                names(categories(ds$interaction)),
                apply(
                    expand.grid(
                        c("Cat", "Dog", "Bird", "Skipped", "Not Asked"),
                        c("Argentina", "Australia", "Austria", "Belgium", "Brazil")
                    ),
                    1, paste,
                    collapse = ":"
                )
            ))
        )
        expect_equal(description(ds$interaction2), "This is a description")
    })
})
