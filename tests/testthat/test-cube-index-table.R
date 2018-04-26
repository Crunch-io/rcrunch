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
        cubify(107.142857142857, 85.7142857142857,
               93.75, 112.5,
               dims = catXcat_names)
    )

    expect_equal(
        index.table(catXcat_cube, 2),
        cubify(107.142857142857, 85.7142857142857,
               93.75, 112.5,
               dims = catXcat_names)
    )
})

mrXcat_cube <- loadCube(test_path("cubes/selected-crosstab-4.json"))
mrXcat_names <- dimnames(mrXcat_cube)
mrXcat_names$pdl_gender <- mrXcat_names$pdl_gender[mrXcat_names$pdl_gender != "No Data"]

test_that("index table on a bivariate cube with mr", {
    expect_equal(
        index.table(mrXcat_cube, 1),
        cubify(95.8651525855387, 103.859044435659,
               102.305106635277, 97.8432045727022,
               97.6031146323114, 102.24274029149,
               98.1029444304978, 101.829068203842,
               114.466510106092, 86.0656684647625,
               99.2925720053358, 100.682933745397,
               dims = mrXcat_names)
        )

    expect_equal(
        index.table(mrXcat_cube, 2),
        cubify(95.8651525855387, 103.859044435659,
               102.305106635277, 97.8432045727022,
               97.6031146323114, 102.24274029149,
               98.1029444304978, 101.829068203842,
               114.466510106092, 86.0656684647625,
               99.2925720053358, 100.682933745397,
               dims = mrXcat_names)
    )})
