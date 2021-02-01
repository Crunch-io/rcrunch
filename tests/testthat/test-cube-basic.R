context("Basic cube methods")

v7 <- v7_ifany <- v7_always <- loadCube("cubes/univariate-categorical.json")
v7_ifany@useNA <- "ifany"
v7_always@useNA <- "always"
v4_x_v7 <- v4_x_v7_ifany <- v4_x_v7_always <- loadCube("cubes/cat-x-cat.json")
v4_x_v7_ifany@useNA <- "ifany"
v4_x_v7_always@useNA <- "always"

test_that("useNA on univariate cube", {
    expect_equal(
        as.array(v7),
        cubify(10, 5,
            dims = list(
                v7 = c("C", "E")
            )
        )
    )
    expect_equal(
        as.array(v7_ifany),
        cubify(10, 5, 5,
            dims = list(
                v7 = c("C", "D", "E")
            )
        )
    )
    expect_equal(
        as.array(v7_always),
        cubify(10, 5, 5, 0,
            dims = list(
                v7 = c(LETTERS[3:5], "No Data")
            )
        )
    )
})

test_that("useNA on bivariate cube", {
    expect_equivalent(
        as.array(v4_x_v7),
        cubify(
            5, 2,
            5, 3,
            dims = list(
                v4 = c("B", "C"),
                v7 = c("C", "E")
            )
        )
    )
    expect_equivalent(
        as.array(v4_x_v7_ifany),
        cubify(
            5, 3, 2,
            5, 2, 3,
            dims = list(
                v4 = c("B", "C"),
                v7 = c(LETTERS[3:5])
            )
        )
    )
    expect_equivalent(
        as.array(v4_x_v7_always),
        cubify(
            5, 3, 2, 0,
            5, 2, 3, 0,
            0, 0, 0, 0,
            dims = list(
                v4 = c("B", "C", "No Data"),
                v7 = c(LETTERS[3:5], "No Data")
            )
        )
    )
})

test_that("Cube print method", {
    expect_prints(
        v4_x_v7_ifany,
        paste("   v7",
            "v4  C D E",
            "  B 5 3 2",
            "  C 5 2 3",
            sep = "\n"
        )
    )
})

selected_subvar <- loadCube("cubes/univariate-categorical-like-selected.json")

test_that("Categorical with categories Selected, Not selected", {
    # when detecting if a dimension is selected, we look at the categories. This
    # breaks if one tries to display a categorical that just so happens to have
    # Selected and Not selected as categories
    expect_prints(
        selected_subvar,
        paste("selected_like",
            "    Selected Not selected ",
            "          10            5 ",
            sep = "\n"
        )
    )
})

with_mock_crunch({
    ds <- loadDataset("test ds")

    test_that("useNA is set correctly on CrunchCube init", {
        expect_identical(
            crtabs(~ birthyr > 1980, data = ds)@useNA,
            "no"
        )
        expect_identical(
            crtabs(~ birthyr > 1980, data = ds, useNA = "ifany")@useNA,
            "ifany"
        )
        expect_identical(
            crtabs(~ birthyr > 1980, data = ds, useNA = "always")@useNA,
            "always"
        )
        expect_error(
            crtabs(~ birthyr > 1980, data = ds, useNA = "fosho"),
            "should be one of"
        )
    })

    test_that("cubeMeasureType works correctly", {
        expect_identical(
            cubeMeasureType(crtabs(~ birthyr > 1980, data = ds)),
            "count"
        )

        expect_identical(
            cubeMeasureType(crtabs(min(birthyr) ~ 1, data = ds)),
            "min"
        )
    })
})


# Default measure
test_that("Defualt measure works as expected", {
    # easy case
    expect_equal(
        default_measure_helper(c("count", ".unweighted_counts"), c("count", ".unweighted_counts")),
        "count"
    )
    # prefers count over alphabet
    expect_equal(
        default_measure_helper(c("aaa", "count", ".unweighted_counts"), c("a", "b", ".unweighted_counts")),
        "b"
    )
    # prefers mean over alphabet
    expect_equal(
        default_measure_helper(c("aaa", "mean", ".unweighted_counts"), c("a", "b", ".unweighted_counts")),
        "b"
    )
    # prefers alphabetical count over mean and other count
    expect_equal(
        default_measure_helper(
            c("count", "mean", "count", ".unweighted_counts"),
            c("z", "a", "b", ".unweighted_counts")
        ),
        "b"
    )
    # prefers anything over unweighted count
    expect_equal(
        default_measure_helper(c("aaa", ".unweighted_counts"), c("a", ".unweighted_counts")),
        "a"
    )
    # but will give .unweighted counts if only one
    expect_equal(
        default_measure_helper(c(".unweighted_counts"), c(".unweighted_counts")),
        ".unweighted_counts"
    )
})
