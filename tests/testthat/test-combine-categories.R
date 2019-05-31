context("Combine categories")

with_mock_crunch({
    ds <- loadDataset("test ds")

    both <- VariableDefinition(
        name = "Gender 1 cat",
        description = "Gender",
        discarded = FALSE,
        notes = "",
        format = list(summary = list(digits = 2)),
        view = list(
            include_missing = FALSE,
            show_counts = FALSE,
            show_codes = FALSE,
            column_width = NULL
        ),
        derivation = list(
            `function` = "combine_categories",
            args = list(
                list(variable = "https://app.crunch.io/api/datasets/1/variables/gender/"),
                list(value = list(
                    list(
                        name = "Both",
                        combined_ids = I(c(1, 2)),
                        missing = FALSE,
                        numeric_value = NULL,
                        id = 1
                    ),
                    list(
                        numeric_value = NULL,
                        missing = TRUE,
                        id = -1,
                        name = "No Data",
                        combined_ids = I(-1)
                    )
                ))
            )
        )
    )
    test_that("combine() constructs the correct VarDef for categorical", {
        combine.names <- combine(ds$gender,
            name = "Gender 1 cat",
            list(list(name = "Both", categories = c("Male", "Female")))
        )
        expect_json_equivalent(combine.names, both)
        expect_json_equivalent(combine.names$expr, both$expr)
        combine.ids <- combine(ds$gender,
            name = "Gender 1 cat",
            list(list(name = "Both", categories = c(1, 2)))
        )
        expect_json_equivalent(combine.ids, both)
    })
    test_that("combineCategories() is an alias for combine()", {
        combine.names <- combineCategories(ds$gender,
            name = "Gender 1 cat",
            list(list(name = "Both", categories = c("Male", "Female")))
        )
        expect_json_equivalent(combine.names, both)
    })
    test_that("combineResponses() is only for MR", {
        expect_error(combineResponses(ds$gender),
            "combineResponses.. is only available for Multiple Response variables")
    })

    test_that("Default variable name for combine()", {
        expect_identical(
            combine(
                ds$gender,
                list(list(name = "Both", categories = c("Male", "Female")))
            )$name,
            "Gender (1 category)"
        )
        expect_identical(
            combine(ds$gender)$name,
            "Gender (2 categories)"
        )
    })

    test_that("combine() validation on variable type", {
        expect_error(
            combine(ds$birthyr),
            paste0(
                "Cannot combine ", dQuote("Birth Year"), ": must be type ",
                "categorical, categorical_array, or multiple_response"
            )
        )
        expect_error(
            combine(ds$starttime),
            "categorical, categorical_array, or multiple_response"
        )
    })

    test_that("combine() requires combinations", {
        expect_error(
            combine(ds$gender, combinations = 45),
            "'combinations' must be a list of combination specifications"
        )
        expect_error(
            combine(ds$gender,
                combinations = list(name = "Both", categories = c("Male", "Female"))
            ),
            "'combinations' must be a list of combination specifications"
        )
        expect_error(
            combine(ds$gender,
                combinations = list(list(name = "Both"))
            ),
            "'combinations' must be a list of combination specifications"
        )
    })

    test_that("combinations must have valid names", {
        expect_error(
            combine(
                ds$gender,
                list(
                    list(name = "Man", categories = 1),
                    list(name = "Man", categories = 2)
                )
            ),
            paste("Duplicate category name given:", dQuote("Man"))
        )
        expect_error(
            combine(
                ds$gender,
                list(list(name = "Male", categories = 2))
            ),
            paste("Duplicate category name given:", dQuote("Male"))
        )
        expect_error(
            combine(
                ds$gender,
                list(list(name = "Male", categories = 1))
            ),
            NA
        ) ## "Male" is category in original but not result
    })

    test_that("combinations reference unique categories", {
        expect_error(
            combine(ds$gender,
                name = "Gender 1 cat",
                list(
                    list(name = "Both", categories = c("Male", "Female")),
                    list(name = "Men", categories = 1)
                )
            ),
            paste(
                "Category", dQuote("Male"),
                "referenced in multiple combinations"
            )
        )
        expect_error(
            combine(ds$gender,
                name = "Gender 1 cat",
                list(
                    list(name = "Both", categories = c("Male", "Female")),
                    list(name = "Men", categories = 1),
                    list(name = "Ladies", categories = "Female")
                )
            ),
            paste(
                "Categories", dQuote("Male"), "and", dQuote("Female"),
                "referenced in multiple combinations"
            )
        )
    })

    test_that("combinations reference actual categories", {
        expect_error(
            combine(ds$gender,
                name = "Gender 1 cat",
                list(list(name = "Both", categories = c(1, 42)))
            ),
            paste(
                "Combination", dQuote("Both"),
                "references category with id 42, which does not exist"
            )
        )
        expect_error(
            combine(ds$gender,
                name = "Gender 1 cat",
                list(list(name = "Both", categories = c("Male", "Not male")))
            ),
            paste("Category not found:", dQuote("Not male"))
        )
    })

    test_that("combinations reference categories by name or id (char or numeric)", {
        expect_error(
            combine(ds$gender,
                name = "Gender 1 cat",
                list(list(name = "Both", categories = list("Male", "Female")))
            ),
            "Combinations must reference 'categories' by name or id"
        )
        expect_error(
            combine(ds$gender,
                name = "Gender 1 cat",
                list(list(name = "Both", categories = NULL))
            ),
            "Combinations must reference 'categories' by name or id"
        )
    })

    test_that("collapseCategories errors correctly", {
        expect_error(
            collapseCategories(ds$birthyr, "from", "to"),
            "Variable must be a categorical."
        )
        expect_error(
            collapseCategories(ds$gender, "Male", 1),
            "Destination category must be a character string of length 1."
        )
        expect_error(
            collapseCategories(ds$gender, "Male", c("young", "old")),
            "Destination category must be a character string of length 1."
        )
        expect_error(
            collapseCategories(ds$gender, 1, "young"),
            paste0(dQuote("from"), " must be a character vector.")
        )
        expect_error(
            collapseCategories(ds$gender, "female", "woman"),
            "female is not present in variable categories."
        )
    })
    test_that("collapseCategories takes no action when from and to are the same", {
        expect_no_request(collapseCategories(ds$location, "London", "London"))
    })
    test_that("collapseCategories updates categories when merging into a new category", {
        expect_PATCH(
            var <- collapseCategories(ds$location, c("London", "Scotland"), "GB"),
            "https://app.crunch.io/api/datasets/1/variables/location/",
            '{"categories":[{"id":1,"missing":false,"name":"London","numeric_value":1},{"id":2,"missing":false,"name":"Scotland","numeric_value":2},{"id":-1,"missing":true,"name":"No Data","numeric_value":null},{"id":3,"name":"GB"}]}'
        )
    })
    test_that("collapseCategories updates the variable", {
        expect_POST(
            var <- collapseCategories(ds$location, "Scotland", "London"),
            "https://app.crunch.io/api/datasets/1/table/",
            '{"command":"update","variables":{',
            '"https://app.crunch.io/api/datasets/1/variables/location/":{"value":1}},',
            '"filter":{"function":"selected","args":[{"function":"in","args":[',
            '{"variable":"https://app.crunch.io/api/datasets/1/variables/location/"},',
            '{"column":[2]}]}]}}'
        )
    })
    test_that("collapseCategories modifies category name", {
        expect_PATCH(
            categories(ds$gender) <- collapseCategories(ds$gender, "Female", "woman"),
            "https://app.crunch.io/api/datasets/1/variables/gender/",
            '{"categories":[{"id":1,"missing":false,"name":"Male","numeric_value":1},{"id":2,"missing":false,"name":"woman","numeric_value":2},{"id":-1,"missing":true,"name":"No Data","numeric_value":null}]}'
        )
    })
})

with_test_authentication({
    ds <- newDatasetFromFixture("apidocs")
    ds$combined_pets <- combine(ds$q1,
        name = "Pets (combined)",
        list(list(name = "Mammals", categories = c("Cat", "Dog")))
    )
    test_that("We can create a new categorical by combining", {
        expect_identical_temp_nodata(
            names(categories(ds$combined_pets)),
            c("Mammals", "Bird", "Skipped", "Not Asked", "No Data")
        )
        expect_equivalent(
            as.array(crtabs(~q1, data = ds)),
            array(c(6, 4, 3),
                dim = 3,
                dimnames = list(q1 = c("Cat", "Dog", "Bird"))
            )
        )
        expect_equivalent(
            as.array(crtabs(~combined_pets, data = ds)),
            array(c(10, 3),
                dim = 2,
                dimnames = list(combined_pets = c("Mammals", "Bird"))
            )
        )
    })

    test_that("Updating values in the parent variable updates in the derivation too", {
        ds$q1[is.na(ds$q1)] <- "Bird"
        expect_equivalent(
            as.array(crtabs(~q1, data = ds)),
            array(c(6, 4, 10),
                dim = 3,
                dimnames = list(q1 = c("Cat", "Dog", "Bird"))
            )
        )
        expect_equivalent(
            as.array(crtabs(~combined_pets, data = ds)),
            array(c(10, 10),
                dim = 2,
                dimnames = list(combined_pets = c("Mammals", "Bird"))
            )
        )
        expect_equivalent(
            as.array(crtabs(~q1, data = ds)),
            array(c(6, 4, 10),
                dim = 3,
                dimnames = list(q1 = c("Cat", "Dog", "Bird"))
            )
        )
        expect_equivalent(
            as.array(crtabs(~combined_pets, data = ds)),
            array(c(10, 10),
                dim = 2,
                dimnames = list(combined_pets = c("Mammals", "Bird"))
            )
        )
    })

    test_that("combine() with no combinations is effectively a copy", {
        ds$combined_pets2 <- combine(ds$q1)
        expect_identical(as.vector(ds$combined_pets2), as.vector(ds$q1))
    })

    test_that("combine() with categorical array", {
        ds$combined_petloc <- combine(ds$petloc,
            name = "Pet locations (combined)",
            list(list(name = "Mammals", categories = c("Cat", "Dog")))
        )
        expect_identical_temp_nodata(
            names(categories(ds$combined_petloc)),
            c("Mammals", "Bird", "Skipped", "Not Asked", "No Data")
        )
    })

    test_that("collapseCategories works on categorical variable", {
        ds$cat <- factor(rep(c("cat", "Cat", "dog", "aphid"), 5))
        expect_identical(names(categories(ds$cat)), c("Cat", "aphid", "cat", "dog", "No Data"))
        ds$cat <- collapseCategories(ds$cat, c("cat", "Cat"), "cat")
        expect_identical(names(categories(ds$cat)), c("aphid", "cat", "dog", "No Data"))
        ds$cat <- collapseCategories(ds$cat, c("cat", "aphid"), "nope")
        expect_identical(names(categories(ds$cat)), c("dog", "No Data", "nope"))
        ds$cat <- collapseCategories(ds$cat, c("dog", "nope"), "Has Data")
        expect_identical(names(categories(ds$cat)), c("No Data", "Has Data"))
        expect_identical(names(table(ds$cat, useNA = "always")), c("No Data", "Has Data"))
    })
    test_that("collapseCategories renames variable", {
        # when length(from) == 1 and to is not present in the categories, the categories are just renamed
        ds$q1 <- collapseCategories(ds$q1, "Bird", "Tucan")
        expect_identical_temp_nodata(names(categories(ds$q1)), c("Cat", "Dog", "Tucan", "Skipped", "Not Asked", "No Data"))
    })
})
