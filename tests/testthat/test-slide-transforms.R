
ca_cube <- loadCube("cubes/cat-array.json")
mr_x_mr_cube <- loadCube("cubes/mr-by-mr-different-mrs.json")

test_that("slideTransform() makes a SlideTransforms object", {
    slide_transform <- slideTransform(palette = "foo")
    expect_true(inherits(slide_transform, "SlideTransform"))
    expect_equal(slide_transform$palette, "foo")
})

test_that("prepareSlideTransform() can convert element components", {
    transform <- prepareSlideTransform(
        slideTransform(
            palette = c("#FFFFFF", "#BBBBBB"),
            hide = c("Neutral"),
            renames = c("A little unhappy" = 4),
            order = c("Extremely Unhappy", "Somewhat Unhappy", "Somewhat Happy", "Extremely Happy"),
            insertions = list("insertion"),
            name = "Test",
            description = "Description",
            other = "foo"
        ),
        "columns_dimension",
        ca_cube
    )

    expected <- list(
        order = c(5, 4, 2, 1),
        insertions = list("insertion"),
        name = "Test",
        description = "Description",
        other = "foo",
        elements = list(
            `3` = list(hide = TRUE),
            `4` = list(name = "A little unhappy", fill = "#BBBBBB"),
            `5` = list(fill = "#FFFFFF")
        )
    )

    expect_equal(transform, expected)
})

test_that("prepareSlideTransform() can handle pre-specified elements", {
    transform <- prepareSlideTransform(
        slideTransform(
            elements = list("foo"),
            name = "Test"
        ),
        "columns_dimension",
        ca_cube
    )

    expected <- list(
        name = "Test",
        elements = list("foo")
    )

    expect_equal(transform, expected)
})

test_that("prepareSlideTransform() can handle no elements", {
    transform <- prepareSlideTransform(
        slideTransform(
            order = c("Extremely Unhappy", "Somewhat Unhappy", "Somewhat Happy", "Extremely Happy"),
        ),
        "columns_dimension",
        ca_cube
    )

    expected <- list(order = c(5, 4, 2, 1))

    expect_equal(transform, expected)
})

test_that("prepareSlideTransform() checks arguments", {
    expect_error(
        prepareSlideTransform(slideTransform(elements = list("foo"), renames = c("a" = "Extremely Happy"))),
        "Cannot specify `palette`, `renames`, or `hide` if `elements` is provided"
    )

    expect_error(
        prepareSlideTransform(slideTransform(name = "test"), "xyz", "columns_dimension"),
        "Expected slideTransform dim to be one of"
    )
})

test_that("getDimIDCrosswalk() handles CA categories dimension", {
    expect_equivalent(
        getDimIDCrosswalk(ca_cube, 2),
        data.frame(
            id = c(1:5, -1),
            name = c(
                "Extremely Happy", "Somewhat Happy", "Neutral", "Somewhat Unhappy",
                "Extremely Unhappy", "No Data"
            ),
            stringsAsFactors = FALSE
        )
    )
})

test_that("getDimIDCrosswalk() handles CA items dimension", {
    expect_equivalent(
        getDimIDCrosswalk(ca_cube, 1),
        data.frame(
            id = c("cat_feeling", "dog_feeling"),
            alias = c("cat_feeling", "dog_feeling"),
            name = c("cat_feeling", "dog_feeling"),
            stringsAsFactors = FALSE
        )
    )
})

test_that("getDimIDCrosswalk() handles MR items dimension", {
    expect_equivalent(
        getDimIDCrosswalk(mr_x_mr_cube, 1),
        data.frame(
            id = c("food_opinion__1", "rest_opinion__1", "play_opinion__1"),
            alias = c("food_opinion__1", "rest_opinion__1", "play_opinion__1"),
            name = c("food_opinion", "rest_opinion", "play_opinion"),
            stringsAsFactors = FALSE
        )
    )
})

test_that("standardizeTransformIDs() standardizes simple case", {
    standardized <- standardizeTransformIDs(
        c("new a" = "a", "new b" = "b"),
        data.frame(
            id = 1:2,
            name = c("a", "b")
        ),
        "foo"
    )
    expect_equal(standardized, c("new a" = 1L, "new b" = 2L))
})

test_that("standardizeTransformIDs() chooses right column to standardize", {
    standardized <- standardizeTransformIDs(
        c("new a" = "a", "new b" = "b"),
        data.frame(
            id = 1:2,
            name = c("a", "c"),
            other_name = c("b", "a")
        ),
        "foo"
    )
    expect_equal(standardized, c("new a" = 2L, "new b" = 1L))
})

test_that("standardizeTransformIDs() chooses ok if duplicate standardize columns", {
    standardized <- standardizeTransformIDs(
        c("new a" = "a", "new b" = "b"),
        data.frame(
            id = 1:2,
            name = c("a", "b"),
            other_name = c("b", "a")
        ),
        "foo"
    )
    expect_equal(standardized, c("new a" = 1L, "new b" = 2L))
})

test_that("standardizeTransformIDs() provides informative error for no match", {
    expect_error(
        standardizeTransformIDs(
            c("new a" = "a", "new c" = "c"),
            data.frame(
                id = 1:2,
                name = c("a", "b"),
                other_name = c("d", "e")
            ),
            "foo"
        ),
        "Could not match transform ids for foo to a set of expected values:\n  - name: c\n  - other_name: All"
    )
})

test_that("standardizeTransformIDs() ignores non-character", {
    expect_equal(
        standardizeTransformIDs(1:3, data.frame(), "foo"),
        1:3
    )
    expect_equal(
        standardizeTransformIDs(NULL, data.frame(), "foo"),
        NULL
    )
})
