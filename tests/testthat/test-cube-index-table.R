context("Index tables")

uni_cube <- loadCube(test_path("cubes/univariate-categorical.json"))
catXmrXmr_cube <- loadCube(test_path("cubes/cat-x-mr-x-mr.json"))

test_that("index table validation", {
    expect_error(index.table(uni_cube, 1),
                 "Index tables can only be calculated for 2 dimensional cubes.")
    expect_error(index.table(catXmrXmr_cube, 1),
                 "Index tables can only be calculated for 2 dimensional cubes.")
})

catXcat_cube <- loadCube(test_path("cubes/cat-x-cat.json"))
catXcat_names <- dimnames(catXcat_cube)
catXcat_names$v4 <- catXcat_names$v4[catXcat_names$v4 != "No Data"]
catXcat_names$v7 <- catXcat_names$v7[!catXcat_names$v7 %in% c("D", "No Data")]

test_that("index table on a bivariate cube", {
    expect_equal(
        index.table(catXcat_cube, 1),
        cubify(1.07142857142857, 0.857142857142857,
               0.9375, 1.125,
               dims = catXcat_names)
    )

    expect_equal(
        index.table(catXcat_cube, 2),
        cubify(1.07142857142857, 0.857142857142857,
               0.9375, 1.125,
               dims = catXcat_names)
    )
})

mrXcat_cube <- loadCube(test_path("cubes/selected-crosstab-4.json"))
mrXcat_names <- dimnames(mrXcat_cube)
mrXcat_names$pdl_gender <- mrXcat_names$pdl_gender[mrXcat_names$pdl_gender != "No Data"]

test_that("index table on a bivariate cube with mr", {
    expect_equal(
        index.table(mrXcat_cube, 1),
        cubify(0.958651525855387, 1.03859044435659,
               1.02305106635277, 0.978432045727022,
               0.976031146323114, 1.0224274029149,
               0.981029444304978, 1.01829068203842,
               1.14466510106092, 0.860656684647625,
               0.992925720053358, 1.00682933745397,
               dims = mrXcat_names)
        )

    expect_equal(
        index.table(mrXcat_cube, 2),
        cubify(0.958651525855387, 1.03859044435659,
               1.02305106635277, 0.978432045727022,
               0.976031146323114, 1.0224274029149,
               0.981029444304978, 1.01829068203842,
               1.14466510106092, 0.860656684647625,
               0.992925720053358, 1.00682933745397,
               dims = mrXcat_names)
    )})
