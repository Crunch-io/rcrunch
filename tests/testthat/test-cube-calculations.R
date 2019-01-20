context("Cube calculation attributes")

test_that("dimensions attribute is correct on multiple MR cube", {
    cube <- loadCube("cubes/cat-x-mr-x-mr.json")
    marg <- margin.table(cube, 1:2)
    expect_equal(names(attr(marg, "dims")), names(dimnames(marg)))

    marg <- margin.table(cube, 1)
    expect_equal(names(attr(marg, "dims")), names(dimnames(marg)))

    marg <- margin.table(cube, 2)
    expect_equal(names(attr(marg, "dims")), names(dimnames(marg)))

    marg <- margin.table(cube, 3)
    expect_equal(names(attr(marg, "dims")), names(dimnames(marg)))

    marg <- margin.table(cube, 2:3)
    expect_equal(names(attr(marg, "dims")), names(dimnames(marg)))

    marg <- margin.table(cube, 1:3)
    expect_equal(names(attr(marg, "dims")), names(dimnames(marg)))
})


cube <- loadCube("cubes/selected-crosstab-4.json")
marg <- margin.table(cube, 1)
test_that("margin table returns a cube-calculation", {
    expect_is(marg, "CrunchCubeCalculation")
    expect_equal(attr(marg, "type"), "margin")
    dims <- attr(marg, "dims")
    expect_is(dims, "CubeDims")
    expect_equal(length(dims), length(dim(marg)))
})

prop <- prop.table(cube, 1)
test_that("prop.table returns a cube-calculation", {
    expect_is(prop, "CrunchCubeCalculation")
    expect_equal(attr(prop, "type"), "proportion")
    dims <- attr(prop, "dims")
    expect_is(dims, "CubeDims")
    expect_equal(length(dims), length(dim(prop)))
})

prop_table <- prop.table(loadCube("cubes/univariate-categorical.json"))
test_that("prop.table with no margin, on a univariate returns a cube-calculation", {
    expect_is(prop_table, "CrunchCubeCalculation")
    expect_equal(attr(prop_table, "type"), "proportion")
    dims <- attr(prop_table, "dims")
    expect_is(dims, "CubeDims")
    expect_equal(length(dims), length(dim(prop_table)))
})

test_that("prop.table with a variety of arguments returns a cube-calculation", {
    for (m in c(NULL, 1:2)) {
        prop <- prop.table(loadCube("cubes/selected-crosstab-4.json"), m)
        expect_is(prop, "CrunchCubeCalculation")
        expect_equal(attr(prop, "type"), "proportion")
        dims <- attr(prop, "dims")
        expect_is(dims, "CubeDims")
        expect_equal(length(dims), length(dim(prop)))
    }
})

test_that("as.array method", {
    arr <- as.array(marg)
    expect_is(arr, "array")
    expect_null(attr(arr, "type"))
    expect_null(attr(arr, "dims"))
    expect_is(as.array(prop), "matrix")
})

