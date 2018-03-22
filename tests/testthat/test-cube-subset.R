context("Cube subsets")

test_that("subset by list", {
    expect_identical(subsetByList(mtcars, list(1,1), drop = FALSE),
        mtcars[1,1, drop = FALSE])
    expect_identical(subsetByList(mtcars, alist(,1), drop = TRUE),
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

test_that("[ method for cat by cat cubes", {
    cube <- cube_withNA <- loadCube(test_path("cubes/cat-x-cat.json"))
    cube_withNA@useNA <- "always"
    subset_cube <- cube[1:2,] # drop the No Data row
    expect_is(subset_cube, "CrunchCube")
    expect_equal(dim(subset_cube), c(2, 4))
    expect_equal(as.array(subset_cube), as.array(cube)[1:2, ])

    # drop a non-missing row
    subset_cube <- cube[c(1, 3),] # drop the second row (C)
    expect_is(subset_cube, "CrunchCube")
    expect_equal(dim(subset_cube), c(2, 4))
    # since the cube retains the missing dimension, it defaults behavior to
    # `drop = FALSE`, though this isn't true of the the array since only one
    # element is being selected from that dimension
    expect_equal(as.array(subset_cube), as.array(cube)[1, , drop = FALSE])

    # check that useNA doesn't impact
    subset_cube_withNA <- cube_withNA[c(1, 3),] # drop the second row (C)
    expect_equal(as.array(subset_cube_withNA), as.array(cube_withNA)[c(1, 3),])

    # drop, selecting a missing category on columns
    drop_cube <- cube[1, 1:2]
    expect_is(subset_cube, "CrunchCube")
    expect_equal(dim(drop_cube), 2)
    # fails because the second item in columns is actually hidden. The
    # subsetting selected it and then useNA isn't being respected. The first
    # expect_equal below is the one I thought should work
    expect_equal(as.array(drop_cube), as.array(cube)[1, 1])

    # no drop, selecting a missing category on columns
    no_drop_cube <- cube[1, 1:2, drop = FALSE]
    expect_is(no_drop_cube, "CrunchCube")
    expect_equal(dim(no_drop_cube), c(1, 2))
    expect_equal(as.array(no_drop_cube), as.array(cube)[1, 1, drop = FALSE])

    # no drop, selecting a non-missing categories on columns
    no_drop_cube <- cube[1, c(1,3), drop = FALSE]
    expect_is(no_drop_cube, "CrunchCube")
    expect_equal(dim(no_drop_cube), c(1, 2))
    expect_equal(as.array(no_drop_cube), as.array(cube)[1, 1:2, drop = FALSE])
})

test_that("[ method for MR cubes", {
    cat_x_mr_x_mr_withNA <- cat_x_mr_x_mr
    cat_x_mr_x_mr_withNA@useNA <- "always"

    # subset rows
    # drop the No Data row which is #2 here!
    subset_cat_x_mr_x_mr <- cat_x_mr_x_mr[c(1,3), , ]
    expect_is(subset_cat_x_mr_x_mr, "CrunchCube")
    expect_equal(dim(subset_cat_x_mr_x_mr), c(2, 3, 2))
    expect_equal(as.array(subset_cat_x_mr_x_mr), as.array(cat_x_mr_x_mr)[1:2, , ])

    subset_cat_x_mr_x_mr_withNA <- cat_x_mr_x_mr_withNA[c(1, 3), , ]
    expect_equal(as.array(subset_cat_x_mr_x_mr_withNA), as.array(cat_x_mr_x_mr_withNA)[c(1, 3), , ])
    subset_cat_x_mr_x_mr_withNA <- cat_x_mr_x_mr_withNA[c(1, 2), , ]
    expect_equal(as.array(subset_cat_x_mr_x_mr_withNA), as.array(cat_x_mr_x_mr_withNA)[c(1, 2), , ])

    # subset cols
    # drop the No Data row which is #2 here!
    subset_cat_x_mr_x_mr <- cat_x_mr_x_mr[, c(1,3), ]
    expect_is(subset_cat_x_mr_x_mr, "CrunchCube")
    expect_equal(dim(subset_cat_x_mr_x_mr), c(3, 2, 2))
    expect_equal(as.array(subset_cat_x_mr_x_mr), as.array(cat_x_mr_x_mr)[, c(1,3), ])

    subset_cat_x_mr_x_mr_withNA <- cat_x_mr_x_mr_withNA[, c(1, 3), ]
    expect_equal(as.array(subset_cat_x_mr_x_mr_withNA), as.array(cat_x_mr_x_mr_withNA)[, c(1, 3), ])
    subset_cat_x_mr_x_mr_withNA <- cat_x_mr_x_mr_withNA[, c(1, 2), ]
    expect_equal(as.array(subset_cat_x_mr_x_mr_withNA), as.array(cat_x_mr_x_mr_withNA)[, c(1, 2), ])

    # subset cols with drop
    subset_cat_x_mr_x_mr <- cat_x_mr_x_mr[, 3, ]
    expect_is(subset_cat_x_mr_x_mr, "CrunchCube")
    expect_equal(dim(subset_cat_x_mr_x_mr), c(3, 2))
    expect_equal(as.array(subset_cat_x_mr_x_mr), as.array(cat_x_mr_x_mr)[, 3, ])

    subset_cat_x_mr_x_mr_withNA <- cat_x_mr_x_mr_withNA[, 3, ]
    expect_equal(as.array(subset_cat_x_mr_x_mr_withNA), as.array(cat_x_mr_x_mr_withNA)[, 3, ])
    subset_cat_x_mr_x_mr_withNA <- cat_x_mr_x_mr_withNA[, 2, ]
    expect_equal(as.array(subset_cat_x_mr_x_mr_withNA), as.array(cat_x_mr_x_mr_withNA)[, 2, ])

    # subset slices
    subset_cat_x_mr_x_mr <- cat_x_mr_x_mr[, , 2]
    expect_is(subset_cat_x_mr_x_mr, "CrunchCube")
    expect_equal(dim(subset_cat_x_mr_x_mr), c(3, 3))
    expect_equal(as.array(subset_cat_x_mr_x_mr), as.array(cat_x_mr_x_mr)[, , 2])

    subset_cat_x_mr_x_mr_withNA <- cat_x_mr_x_mr_withNA[, , 2]
    expect_equal(as.array(subset_cat_x_mr_x_mr_withNA), as.array(cat_x_mr_x_mr_withNA)[, , 2])
})

test_that("[ method for cat array cubes", {
    cube <- cube_withNA <- loadCube(test_path("cubes/catarray-x-cat.json"))
    cube_withNA@useNA <- "always"

    # subset rows
    # drop the No Data row which is #2 here!
    subset_cube <- cube[2, , ]
    expect_is(subset_cube, "CrunchCube")
    expect_equal(dim(subset_cube), c(6, 3))
    expect_equal(as.array(subset_cube), as.array(cube)[2, , ])

    subset_cube_withNA <- cube_withNA[2, , ]
    expect_equal(as.array(subset_cube_withNA), as.array(cube_withNA)[2, , ])

    # subset cols
    subset_cube <- cube[, c(1,3), ]
    expect_is(subset_cube, "CrunchCube")
    expect_equal(dim(subset_cube), c(2, 2, 3))
    expect_equal(as.array(subset_cube), as.array(cube)[, c(1,3), ])

    subset_cube_withNA <- cube_withNA[, c(1, 3), ]
    expect_equal(as.array(subset_cube_withNA), as.array(cube_withNA)[, c(1, 3), ])
    subset_cube_withNA <- cube_withNA[, c(1, 2), ]
    expect_equal(as.array(subset_cube_withNA), as.array(cube_withNA)[, c(1, 2), ])

    # subset cols with drop
    subset_cube <- cube[, 3, ]
    expect_is(subset_cube, "CrunchCube")
    expect_equal(dim(subset_cube), c(2, 3))
    expect_equal(as.array(subset_cube), as.array(cube)[, 3, ])

    subset_cube_withNA <- cube_withNA[, 3, ]
    expect_equal(as.array(subset_cube_withNA), as.array(cube_withNA)[, 3, ])



    # subset slices
    subset_cube <- cube[, , 1:2]
    expect_is(subset_cube, "CrunchCube")
    expect_equal(dim(subset_cube), c(2, 6, 2))
    expect_equal(as.array(subset_cube), as.array(cube)[, , 1:2])

    subset_cube_withNA <- cube_withNA[, , 1:2]
    expect_equal(as.array(subset_cube_withNA), as.array(cube_withNA)[, , 1:2])
})