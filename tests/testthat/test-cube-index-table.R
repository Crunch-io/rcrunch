context("Index tables")

uni_cube <- loadCube(test_path("cubes/univariate-categorical.json"))
catXmrXmr_cube <- loadCube(test_path("cubes/cat-x-mr-x-mr.json"))

test_that("index table validation", {
    expect_error(
        index.table(uni_cube, 1),
        "Index tables can only be calculated for 2 dimensional cubes."
    )
    expect_error(
        index.table(catXmrXmr_cube, 1),
        "Index tables can only be calculated for 2 dimensional cubes."
    )
})

catXcat_cube <- loadCube(test_path("cubes/cat-x-cat.json"))
catXcat_names <- dimnames(catXcat_cube)
catXcat_names$v4 <- catXcat_names$v4[catXcat_names$v4 != "No Data"]
catXcat_names$v7 <- catXcat_names$v7[!catXcat_names$v7 %in% c("D", "No Data")]

test_that("index table on a bivariate cube", {
    expect_equal(
        index.table(catXcat_cube, 1),
        cubify(
            107.142857142857, 85.7142857142857,
            93.75, 112.5,
            dims = catXcat_names
        )
    )

    expect_equal(
        index.table(catXcat_cube, 2),
        cubify(
            100, 80,
            100, 120,
            dims = catXcat_names
        )
    )
})

test_that("index table with baseline", {
    expect_equal(
        index.table(catXcat_cube, 1, c(0.6, 0.4)),
        cubify(
            119.047619047619, 71.4285714285714,
            104.16666666666667, 93.75,
            dims = catXcat_names
        )
    )

    expect_equal(
        index.table(catXcat_cube, 2, c(0.6, 0.4)),
        cubify(
            83.3333333333333, 66.6666666666667,
            125, 150,
            dims = catXcat_names
        )
    )

    expect_equal(
        index.table(
            catXcat_cube, 2,
            array(c(0.6, 0.4, 0.4, 0.6), dim = c(2, 2))
        ),
        cubify(
            83.3333333333333, 100,
            125, 100,
            dims = catXcat_names
        )
    )
})

mrXcat_cube <- loadCube(test_path("cubes/selected-crosstab-4.json"))
mrXcat_names <- dimnames(mrXcat_cube)
mrXcat_names$pdl_gender <- mrXcat_names$pdl_gender[mrXcat_names$pdl_gender != "No Data"]

test_that("index table on a bivariate cube with mr", {
    expect_equal(
        index.table(mrXcat_cube, 1),
        cubify(
            95.3416155822363, 104.394053238208,
            101.879419344372, 98.2272247381305,
            97.1985863211465, 102.642452778304,
            99.2098729346168, 100.745292805163,
            115.700063998356, 85.1908063256891,
            100.477252947149, 99.5498278652431,
            dims = mrXcat_names
        )
    )

    expect_equal(
        index.table(mrXcat_cube, 2),
        cubify(
            95.8651525855387, 103.859044435659,
            102.305106635277, 97.8432045727022,
            97.6031146323114, 102.24274029149,
            98.1029444304978, 101.829068203842,
            114.466510106092, 86.0656684647625,
            99.2925720053358, 100.682933745397,
            dims = mrXcat_names
        )
    )
})

test_that("index table on an mr x mr compared to calculation with mr cube", {
    mr_x_mr <- loadCube(test_path("cubes/full-cube.json"))
    mr_alone <- loadCube(test_path("cubes/natrep-cube.json"))

    expect_equal(
        index.table(mr_x_mr, 2),
        prop.table(mr_x_mr, 2) /
            broadcast(prop.table(mr_alone), dim = dim(mr_x_mr)) * 100
    )
})

test_that("index.table validation", {
    mean_cube <- loadCube("cubes/mean-age-food_groups-x-pasta.json")
    expect_error(
        index.table(mean_cube, 1),
        paste0(
            "You can't use CrunchCubes with measures other than count. ",
            "The cube you provided included measures: cube_mean"
        )
    )
})

with_test_authentication({
    ds <- newDatasetFromFixture("apidocs")

    test_that("proof: index.table(~x+y, 2) == prop.table(~x+y, 2)/prop.table(~x)*100", {
        bivariate_cube <- crtabs(~allpets + country, data = ds)
        univariate_allpets <- crtabs(~allpets, data = ds)
        univariate_country <- crtabs(~country, data = ds)

        expect_equal(
            index.table(bivariate_cube, 2),
            prop.table(bivariate_cube, 2) /
                broadcast(prop.table(univariate_allpets), dim = dim(bivariate_cube)) * 100
        )

        expect_equal(
            index.table(bivariate_cube, 1),
            prop.table(bivariate_cube, 1) /
                broadcast(prop.table(univariate_country), dim = dim(bivariate_cube)) * 100
        )
    })
})
