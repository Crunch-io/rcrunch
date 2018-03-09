context("Cube subsets")

test_that("subset by list", {
    expect_identical( subsetByList(mtcars, list(1,1), drop = FALSE),
        mtcars[1,1, drop = FALSE])
    expect_identical( subsetByList(mtcars, alist(,1), drop = TRUE),
        mtcars[,1, drop = TRUE])
})

test_that("replaceMissingWithTRUE", {
    expect_identical(replaceMissingWithTRUE(alist(, 1)),
        list(TRUE, 1))
    expect_error(replaceMissingWithTRUE(alist(, a))[[2]],
        "object 'a' not found")
})

cat_x_mr_x_mr <- loadCube(test_path("cubes/cat-x-mr-x-mr.json"))
test_that("subsetArrayDimension", {
    expected <- list(
        name = c("cats", "No Data"),
        any.or.none = c(FALSE, FALSE
        ),
        missing = c(FALSE, TRUE),
        references = list(
            alias = "animal",
            name = "animal",
            type = "categorical",
            categories = list(
                list(numeric_value = 1L,
                    missing = FALSE,
                    id = 1L, name = "cats"),
                list(numeric_value = NULL,
                    missing = TRUE,
                    id = -1L,
                    name = "No Data"),
                list(numeric_value = 2L,
                    missing = FALSE,
                    id = 2L,
                    name = "dogs")
                )
            )
        )
    expect_identical(subsetArrayDimension(cat_x_mr_x_mr@dims$animal, 1:2), expected)
    })

test_that("translateCubeIndex", {
    expect_identical(translateCubeIndex(cat_x_mr_x_mr, alist(1, ,), drop = FALSE),
        alist(1, , TRUE, , TRUE))
    expect_identical(translateCubeIndex(cat_x_mr_x_mr, alist(1:2, 1:2, 1:2), drop = FALSE),
        alist(1:2, 1:2, TRUE, 1:2, TRUE))
    expect_identical(translateCubeIndex(cat_x_mr_x_mr, alist(1:2, 1:2, 1:2), drop = FALSE),
        alist(1:2, 1:2, TRUE, 1:2, TRUE))
    expect_identical(translateCubeIndex(cat_x_mr_x_mr, alist(1:2, 1, 2), drop = FALSE),
        alist(1:2, 1, TRUE, 2, TRUE))
    # MR selection entries are set to index 1 when the indicator is dropped
    expect_identical(translateCubeIndex(cat_x_mr_x_mr, alist(1:2, 1, 2), drop = TRUE),
        alist(1:2, 1, 1, 2, 1))
    expect_identical(
        translateCubeIndex(cat_x_mr_x_mr,
            alist(c(TRUE, FALSE, TRUE), c(TRUE, FALSE, TRUE), c(TRUE, FALSE) ), drop = TRUE),
        alist(c(TRUE, FALSE, TRUE), c(TRUE, FALSE, TRUE), TRUE,c(TRUE, FALSE), TRUE))
    expect_error( translateCubeIndex(cat_x_mr_x_mr, alist(1, 1)),
        "You supplied 2 dimensions to subset a 3 dimensional cube.")
})

test_that("[ method for cubes", {
    cube <- loadCube(test_path("cubes/cat-x-cat.json"))
    subset_cube <- cube[1:2,]
    expect_is(subset_cube, "CrunchCube")
    expect_equal(dim(subset_cube), c(2, 4))
    drop_cube <- cube[1, 1:2]
    expect_is(subset_cube, "CrunchCube")
    expect_equal(dim(drop_cube), 2)
    no_drop_cube <- cube[1, 1:2, drop = FALSE]
    expect_is(no_drop_cube, "CrunchCube")
    expect_equal(dim(no_drop_cube), c(1, 2))
})
