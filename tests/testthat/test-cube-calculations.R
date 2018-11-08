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


cube <- loadCube(test_path("cubes/selected-crosstab-4.json"))
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

test_that("as.array method", {
    arr <- as.array(marg)
    expect_is(arr, "array")
    expect_null(attr(arr, "type"))
    expect_null(attr(arr, "dims"))
    expect_is(as.array(prop), "matrix")
})

