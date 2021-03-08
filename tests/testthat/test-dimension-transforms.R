ca_cube <- loadCube("cubes/cat-array.json")
mr_x_mr_cube <- loadCube("cubes/mr-by-mr-different-mrs.json")

test_that("makeDimTransform() makes a DimensionTransforms object", {
    dim_transform <- makeDimTransform(palette = "foo")
    expect_true(inherits(dim_transform, "DimensionTransform"))
    expect_equal(dim_transform$palette, "foo")
})

test_that("prepareDimTransforms() prepares if needed", {
    expect_equal(
        prepareDimTransforms(
            list(
                rows_dimension = makeDimTransform(palette = c("#FFFFFF")),
                columns_dimension = list("foo")
            ),
            cube = ca_cube
        ),
        list(
            rows_dimension = prepareDimTransform(
                makeDimTransform(palette = c("#FFFFFF")), "rows_dimension", ca_cube
            ),
            columns_dimension = list("foo"),
            version = "1.0"
        )
    )
})

test_that("prepareDimTransforms() doesn't add version to empty", {
    expect_equal(
        prepareDimTransforms(
            list(),
            cube = ca_cube
        ),
        list()
    )
})

test_that("prepareDimTransform() can convert element components", {
    transform <- prepareDimTransform(
        makeDimTransform(
            palette = c("#FFFFFF", "#BBBBBB"),
            hide = c("Neutral"),
            renames = c("A little unhappy" = 4),
            order = c("Extremely Unhappy", "Somewhat Unhappy", "Somewhat Happy", "Extremely Happy"),
            name = "Test",
            description = "Description",
            other = "foo"
        ),
        "columns_dimension",
        ca_cube
    )

    expected <- list(
        order = c(5, 4, 2, 1),
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

test_that("prepareDimTransform() can handle palette functions", {
    transform <- prepareDimTransform(
        makeDimTransform(
            palette = function(cats) ifelse(names(cats) == "Extremely Happy", "#1b7837", "#333333"),
        ),
        "columns_dimension",
        ca_cube
    )

    expected <- list(
        elements = list(
            `1` = list(fill = "#1b7837"),
            `2` = list(fill = "#333333"),
            `3` = list(fill = "#333333"),
            `4` = list(fill = "#333333"),
            `5` = list(fill = "#333333")
        )
    )

    expect_equal(transform, expected)
})

test_that("prepareDimTransform() can convert palette functions and reorder/hides", {
    transform <- prepareDimTransform(
        makeDimTransform(
            palette = function(cats) ifelse(names(cats) == "Extremely Happy", "#1b7837", "#333333"),
            hide = c("Neutral"),
            order = c("Extremely Unhappy", "Somewhat Unhappy", "Somewhat Happy", "Extremely Happy"),
        ),
        "columns_dimension",
        ca_cube
    )

    expected <- list(
        order = c(5, 4, 2, 1),
        elements = list(
            `1` = list(fill = "#1b7837"),
            `2` = list(fill = "#333333"),
            `3` = list(hide = TRUE),
            `4` = list(fill = "#333333"),
            `5` = list(fill = "#333333")
        )
    )

    expect_equal(transform, expected)
})

test_that("prepareDimTransform() can handle pre-specified elements", {
    transform <- prepareDimTransform(
        makeDimTransform(
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

test_that("prepareDimTransform() can handle no elements", {
    transform <- prepareDimTransform(
        makeDimTransform(
            order = c("Extremely Unhappy", "Somewhat Unhappy", "Somewhat Happy", "Extremely Happy"),
        ),
        "columns_dimension",
        ca_cube
    )

    expected <- list(order = c(5, 4, 2, 1))

    expect_equal(transform, expected)
})

test_that("prepareDimTransform() checks arguments", {
    expect_error(
        prepareDimTransform(makeDimTransform(elements = list("foo"), renames = c("a" = "Extremely Happy"))),
        "Cannot specify `palette`, `renames`, or `hide` if `elements` is provided"
    )

    expect_error(
        prepareDimTransform(makeDimTransform(name = "test"), "xyz", "columns_dimension"),
        "Expected dimTransform dim to be one of"
    )
})

test_that("getDimIDCrosswalk() handles CA categories dimension", {
    expect_equivalent(
        getDimIDCrosswalk(ca_cube, 2),
        data.frame(
            id = c(1:5),
            name = c(
                "Extremely Happy", "Somewhat Happy", "Neutral", "Somewhat Unhappy",
                "Extremely Unhappy"
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


with_mock_crunch({
    ds <- loadDataset("Vegetables example")
    deck <- decks(ds)[["deck about transforms"]]

    test_that("Can get empty transform from slide/analyses cat/analysis", {
        expect_equivalent(transforms(deck[[1]]), list())
        expect_equivalent(transforms(analyses(deck[[1]])), list())
        expect_equivalent(transforms(analyses(deck[[1]])[[1]]), list())
    })

    test_that("Can get actual transform from slide/analyses cat/analysis", {
        transform <- list(
            rows_dimension = list(elements = list(`1` = list(hide = TRUE))),
            version = "1.0"
        )
        expect_equal(transforms(deck[[2]]), transform)
        expect_equal(transforms(analyses(deck[[2]])), transform)
        expect_equal(transforms(analyses(deck[[2]])[[1]]), transform)
    })

    test_that("Can set transform on existing slide/analyses cat/analysis", {
        url <- "https://app.crunch.io/api/datasets/veg/decks/dk01/slides/dk01s01/analyses/000001/"
        expected <- paste0(
            '{"element":"shoji:entity","body":{"transform":{',
            '"rows_dimension":{"elements":{"1":{"hide":true}}},',
            '"version":"1.0"}}}'
        )
        transform_list <- list(rows_dimension = makeDimTransform(hide = "No"))

        expect_PATCH(
            transforms(deck[[1]]) <- transform_list,
            url,
            expected
        )
        expect_PATCH(
            transforms(analyses(deck[[1]])) <- transform_list,
            url,
            expected
        )
        expect_PATCH(
            transforms(analyses(deck[[1]])[[1]]) <- transform_list,
            url,
            expected
        )
    })

    test_that("Can remove transform on existing slide/analyses cat/analysis", {
        url <- "https://app.crunch.io/api/datasets/veg/decks/dk01/slides/dk01s02/analyses/000001/"
        expected <- '{"element":"shoji:entity","body":{"transform":{"version":"1.0"}}}'
        expect_PATCH(
            transforms(deck[[2]]) <- NULL,
            url,
            expected
        )
        expect_PATCH(
            transforms(analyses(deck[[2]])) <- NULL,
            url,
            expected
        )
        expect_PATCH(
            transforms(analyses(deck[[2]])[[1]]) <- NULL,
            url,
            expected
        )
    })

    # Because veg dataset captured with queries stabalized, need to set it here.
    # For now do so in a targeted way, but it'd be better to set it globally
    # and get all tests to work that way
    with(temp.option(crunch.stabilize.query = TRUE), {
        test_that("Can create slide with transform using slideTransform helper", {
            transform <- list(rows_dimension = makeDimTransform(palette = "#FFFFFF"))
            expect_POST(
                newSlide(deck, ~healthy_eater, title = "Title", transform = transform),
                "https://app.crunch.io/api/datasets/veg/decks/dk01/slides/",
                '{"element":"shoji:entity",',
                '"body":{"title":"Title",',
                '"subtitle":"",',
                '"analyses":[{"query":{"dimensions":[{"variable":"https://app.',
                'crunch.io/api/datasets/veg/variables/var_06/"}],',
                '"measures":{"count":{"function":"cube_count","args":[]}}},',
                '"display_settings":{"percentageDirection":{"value":"colPct"},',
                '"showEmpty":{"value":false},',
                '"showMean":{"value":false},',
                '"vizType":{"value":"table"},',
                '"countsOrPercents":{"value":"percent"},',
                '"decimalPlaces":{"value":1},',
                '"showSignif":{"value":true},',
                '"currentTab":{"value":0}},"transform":{"rows_dimension":{',
                '"elements":{"1":{"fill":"#FFFFFF"}}},"version":"1.0"}}]}}'
            )
        })
    })
})
