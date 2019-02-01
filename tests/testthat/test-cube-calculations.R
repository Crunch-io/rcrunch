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
    expect_prints(
        marg,
        paste(
            "shower_thoughts_klima_2",
            "       Cupcakes are the best cakes Corgis are the future of dog shows ",
            "                         21453.031                          19390.097 ",
            "    I always ride a penny-farthing           I never look at eclipses ",
            "                         24793.106                          20314.818 ",
            "           I never mess with Texas  I don't mind pickles on my burger ",
            "                          7865.977                          12669.603 ",
            sep = "\n"
            ),
    fixed = TRUE
    )
})

prop <- prop.table(cube, 1)
test_that("prop.table returns a cube-calculation", {
    expect_is(prop, "CrunchCubeCalculation")
    expect_equal(attr(prop, "type"), "proportion")
    dims <- attr(prop, "dims")
    expect_is(dims, "CubeDims")
    expect_equal(length(dims), length(dim(prop)))
    expect_prints(
        prop,
        paste(
            "                                    pdl_gender",
            "shower_thoughts_klima_2                   Male    Female",
            "  Cupcakes are the best cakes        0.4627882 0.5372118",
            "  Corgis are the future of dog shows 0.4945227 0.5054773",
            "  I always ride a penny-farthing     0.4718019 0.5281981",
            "  I never look at eclipses           0.4815647 0.5184353",
            "  I never mess with Texas            0.5616081 0.4383919",
            "  I don't mind pickles on my burger  0.4877166 0.5122834",
            sep = "\n"
        )
    )
})

prop_table <- prop.table(loadCube("cubes/univariate-categorical.json"))
test_that("prop.table with no margin, on a univariate returns a cube-calculation", {
    expect_is(prop_table, "CrunchCubeCalculation")
    expect_equal(attr(prop_table, "type"), "proportion")
    dims <- attr(prop_table, "dims")
    expect_is(dims, "CubeDims")
    expect_equal(length(dims), length(dim(prop_table)))
    expect_prints(
        prop_table,
        paste(
            "v7",
            "        C         E ",
            "0.6666667 0.3333333 ",
            sep = "\n"
        )
    )
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

