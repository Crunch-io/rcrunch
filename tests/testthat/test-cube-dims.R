context("Cube dimensions")

test_that("getDimTypes returns the expected cube dimension types", {
    ca_mr <- loadCube(test_path("cubes/catarray-x-mr.json"))
    expect_equivalent(
        getDimTypes(ca_mr),
        c("ca_items", "ca_categories", "mr_items", "mr_selections")
    )
    cat_cat <- loadCube(test_path("cubes/cat-x-cat.json"))
    expect_equivalent(getDimTypes(cat_cat), c("categorical", "categorical"))
    ca <- loadCube(test_path("cubes/cat-array.json"))
    expect_equivalent(getDimTypes(ca), c("ca_items", "ca_categories"))
    cat_mr_mr <- loadCube(test_path("cubes/cat-x-mr-x-mr.json"))
    expect_equivalent(
        getDimTypes(cat_mr_mr),
        c(
            "categorical", "mr_items", "mr_selections", "mr_items",
            "mr_selections"
        )
    )
    cattarray_cat <- loadCube(test_path("cubes/catarray-x-cat.json"))
    expect_equivalent(
        getDimTypes(cattarray_cat),
        c("ca_items", "ca_categories", "categorical")
    )
})

with_mock_crunch({
    ## Load a ton of cube fixtures via the tab book feature
    ds <- loadDataset("test ds")
    m <- multitables(ds)[[1]]
    with_POST("https://app.crunch.io/api/datasets/1/multitables/apidocs-tabbook/", {
        book <- tabBook(m, data = ds)
    })
    cube <- book[[2]][[2]]
    d <- dimensions(cube)
    test_that("Dimensions of a cube", {
        expect_is(d, "CubeDims")
    })
    test_that("Dimnames", {
        expect_identical(
            dimnames(d),
            list(
                q1 = c("Cat", "Dog", "Bird", "Skipped", "Not Asked"),
                allpets = c("Cat", "Dog", "Bird")
            )
        )
        expect_identical(names(dimnames(d)), c("q1", "allpets"))
    })
    test_that("Additional variable attributes on CubeDims", {
        expect_identical(names(variables(d)), c("Pet", "All pets owned"))
        expect_identical(aliases(variables(d)), c("q1", "allpets"))
        expect_identical(
            descriptions(variables(d)),
            c(
                "What is your favorite pet?",
                "Do you have any of these animals as pets? Please select all that apply."
            )
        )
        expect_identical(
            types(variables(d)),
            c("categorical", "subvariable_items")
        )
    })
    test_that("Getting those additional variable attributes from the cube", {
        expect_identical(names(cube), c("Pet", "All pets owned"))
        expect_identical(aliases(cube), c("q1", "allpets"))
        expect_identical(
            descriptions(cube),
            c(
                "What is your favorite pet?",
                "Do you have any of these animals as pets? Please select all that apply."
            )
        )
        expect_identical(
            types(cube),
            c("categorical", "subvariable_items")
        )
        expect_identical(
            notes(cube),
            c("", "")
        )
    })
    test_that("Getting those attributes from MultitableResult (the column variables)", {
        expect_identical(names(book[[1]]), c("Total", "All pets owned", "Pet"))
        expect_identical(aliases(book[[1]]), c("total", "allpets", "q1"))
        expect_identical(
            descriptions(book[[1]]),
            c(
                "",
                "Do you have any of these animals as pets? Please select all that apply.",
                "What is your favorite pet?"
            )
        )
    })
    test_that("Getting those attributes from TabBookResult (the row/sheet variables)", {
        expect_identical(
            names(book)[2:4],
            c("Pet", "Pets by location", "Number of dogs")
        )
        expect_identical(aliases(book)[2:4], c("q1", "petloc", "ndogs"))
        expect_identical(
            descriptions(book)[2:4],
            c(
                "What is your favorite pet?",
                "Name the kinds of pets you have at these locations.",
                ""
            )
        )
    })

    test_that("Variable metadata retrieved from tuples works on the tuples ", {
        expect_identical(name(ds$gender), "Gender")
        expect_identical(description(ds$starttime), "Interview Start Time")
        expect_identical(alias(ds$gender), "gender")
        expect_identical(id(ds$gender), "66ae9881e3524f7db84970d556c34552")
        expect_identical(notes(ds$gender), "")
        expect_identical(notes(ds$birthyr), "Asked instead of age")
        expect_false(uniformBasis(ds$mymrset))
    })

    test_that("Variable metadata from cubes works on the tuples", {
        catarray_x_mr <- loadCube(test_path("cubes/catarray-x-mr.json"))
        cat_array_var <- variables(catarray_x_mr)[[1]]
        mr_var <- variables(catarray_x_mr)[[3]]

        expect_identical(name(cat_array_var), "feeling CA")
        expect_identical(alias(cat_array_var), "feeling_ca")
        expect_identical(description(cat_array_var), "")
        expect_identical(notes(cat_array_var), "")

        expect_equal(
            aliases(subvariables(cat_array_var)),
            c("cat_feeling", "dog_feeling")
        )
        expect_equal(
            names(subvariables(cat_array_var)),
            c("cat_feeling", "dog_feeling")
        )

        mr_var <- variables(catarray_x_mr)[[3]]
        expect_equal(
            aliases(subvariables(mr_var)),
            c("food_opinion__1", "rest_opinion__1", "play_opinion__1")
        )
        expect_equal(
            names(subvariables(mr_var)),
            c("food_opinion", "rest_opinion", "play_opinion")
        )
    })

    test_that("'measures' metadata", {
        expect_identical(
            names(measures(crtabs(max(birthyr) ~ 1, data = ds))),
            "Birth Year"
        )
        expect_identical(
            names(variables(crtabs(max(birthyr) ~ 1, data = ds))),
            "Birth Year"
        )
        expect_length(measures(crtabs(~ gender + textVar, data = ds)), 0)
        expect_identical(
            names(variables(crtabs(~ gender + textVar, data = ds))),
            c("Gender", "Text variable ftw")
        )
        skip("'mean' doesn't return variable metadata like 'max' does")
        expect_identical(
            names(measures(crtabs(mean(birthyr) ~ gender + textVar, data = ds))),
            "Birth Year"
        )
        expect_identical(
            names(variables(crtabs(mean(birthyr) ~ gender + textVar, data = ds))),
            c("Gender", "Text variable ftw", "Birth Year")
        )
        expect_identical(
            names(measures(
                crtabs(list(mean(birthyr), max(birthyr)) ~ gender + textVar, data = ds)
            )),
            c("Birth Year", "Birth Year")
        )
        expect_identical(
            names(variables(
                crtabs(list(mean(birthyr), max(birthyr)) ~ gender + textVar, data = ds)
            )),
            c("Gender", "Text variable ftw", "Birth Year")
        ) ## De-duped
        expect_identical(
            names(measures(
                crtabs(list(mean(birthyr), max(starttime)) ~ gender + textVar, data = ds)
            )),
            c("Interview Start Time", "Birth Year")
        ) ## Order in the JSON is reversed
        expect_identical(
            names(variables(
                crtabs(list(mean(birthyr), max(starttime)) ~ gender + textVar, data = ds)
            )),
            c("Gender", "Text variable ftw", "Interview Start Time", "Birth Year")
        )
    })
})

with_test_authentication({
    ds <- newDataset(data.frame(x = c(LETTERS[1:10])), name = "test for notes")

    test_that("empty variable description are empty string", {
        expect_equal(description(ds$x), "")
        expect_equal(notes(ds$x), "")
    })

    test_that("empty variable description are empty string in a cube too", {
        cube <- crtabs(~x, ds)
        expect_equal(descriptions(variables(cube)), description(ds$x))
        expect_equal(notes(variables(cube)), notes(ds$x))
    })
})
