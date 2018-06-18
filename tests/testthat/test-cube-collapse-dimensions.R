context("collapse.dimensions can collapse arbitrary cube dimensions")

mr_x_mr <- loadCube(test_path("cubes/full-cube.json"))
mr_x_mr_dims <- dimnames(mr_x_mr)

cat_x_mr <- loadCube(test_path("cubes/selected-crosstab-array-last.json"))
cat_x_mr_dims <- dimnames(cat_x_mr)
# drop the "No Data" category in fruit
cat_x_mr_dims$fruit <- cat_x_mr_dims$fruit[cat_x_mr_dims$fruit != "No Data"]

cat_x_mr_x_mr <- loadCube(test_path("cubes/cat-x-mr-x-mr.json"))
cat_x_mr_x_mr_dims <- dimnames(cat_x_mr_x_mr)
# drop the "No Data" category in animal
cat_x_mr_x_mr_dims$animal <- cat_x_mr_x_mr_dims$animal[cat_x_mr_x_mr_dims$animal != "No Data"]

test_that("collapse.dimensions(mr_x_mr)", {
    expect_equivalent(as.array(collapse.dimensions(mr_x_mr, 1)),
                      # note this is a 1-d output, condensed here for space
                      cubify(
                          13068.9587689331, 20954.7013096216,
                          6650.64488216886, 9401.03672837049,
                          22030.3153523541, 28510.504501164,
                          8638.7700564883, 19611.6905531398,
                          17341.5840147774, 19639.4826050037,
                          14457.4508268457, 9384.25096497542,
                          12949.8527858053, 21053.7004299784,
                          16873.4231466009, 8805.6717582668,
                          9704.93506952685, 11520.0756377588,
                          22750.5942352494, 13060.1623918338,
                          17343.0337039009, 9312.51198396474,
                          15122.0959929982, 23391.9651645574,
                          25919.8945071622, 18011.9795313661,
                          24395.8900036815,
                          dims=mr_x_mr_dims["letters"]))
    
    expect_equivalent(as.array(collapse.dimensions(mr_x_mr, 2)),
                      cubify(
                          19935.9325550077,
                          8815.06513916879,
                          12015.5649554376,
                          7967.67530412789,
                          25379.6151957024,
                          5130.6201930451,
                          604.39533293168,
                          1757.81059587395,
                          3140.06957091528,
                          dims=mr_x_mr_dims["offal"]))
    
    # check against a univariate cube with the same data to confirm this is 
    # actually the univariate, unconditional margin
    mr_x_self <- loadCube(test_path("cubes/natrep-cube.json"))
    expect_equal(collapse.dimensions(mr_x_mr, 2),
                 mr_x_self)  
})

test_that("collapse.dimensions(mr_x_mr) proportions", {
    first_univariate <- collapse.dimensions(mr_x_mr, 1)
    expect_equal(prop.table(first_univariate),
                 # note this is a 1-d output, condensed here for space
                 cubify(
                     0.343005652705866, 0.526457708906739,
                     0.170061597715756, 0.248137953864616,
                     0.575295983274584, 0.722413408984012,
                     0.215158236529976, 0.497733964220733,
                     0.468887081108057, 0.529088607024214,
                     0.381627827326823, 0.238897143997144,
                     0.348351059638057, 0.561748349106473,
                     0.444069716726853, 0.223645636273538,
                     0.247956569586389, 0.292280096307629,
                     0.572613085059897, 0.327067092416562,
                     0.437620721993495, 0.219891051379945,
                     0.356885537375432, 0.548258129233669,
                     0.641905291100413, 0.457714901465151,
                     0.579056506939286,
                     dims=mr_x_mr_dims["letters"]))
    
    expect_equal(prop.table(first_univariate, 1),
                 cubify(1, dims=mr_x_mr_dims["letters"]))
    
    second_univariate <- collapse.dimensions(mr_x_mr, 2)
    expect_equal(prop.table(second_univariate),
                 cubify(
                     0.463523266541455,
                     0.204955939573381,
                     0.279369620764814,
                     0.18525358036403,
                     0.590092392548861,
                     0.119290222551763,
                     0.0140525805968646,
                     0.0408702280223159,
                     0.073008639082322,
                     dims=mr_x_mr_dims["offal"]))
    expect_equal(prop.table(second_univariate, 1),
                 cubify(1, dims=mr_x_mr_dims["offal"]))
    
    # check against a univariate cube with the same data to confirm this is 
    # actually the univariate, unconditional margin
    mr_x_self <- loadCube(test_path("cubes/natrep-cube.json"))
    
    expect_equal(prop.table(collapse.dimensions(mr_x_mr, 2)),
                 prop.table(mr_x_self))
    expect_equal(prop.table(collapse.dimensions(mr_x_mr, 2), 1),
                 prop.table(mr_x_self, 1))
})

test_that("collapse.dimensions(cat_x_mr)", {
    expect_equivalent(as.array(collapse.dimensions(cat_x_mr, 1)),
                      cubify(
                          22.9672704148528, 45.7789165449064, 86.9728287914322,
                          dims=cat_x_mr_dims["zoo"]))
    expect_equivalent(prop.table(collapse.dimensions(cat_x_mr, 1)),
                      cubify(
                          0.138355225153969,0.322687424815186,0.480529823991341,
                          dims=cat_x_mr_dims["zoo"]))
    
    expect_equivalent(prop.table(collapse.dimensions(cat_x_mr, 2)),
                      cubify(
                          0.334028222315447, 0.665971777684553,
                          dims=list(fruit = list("rambutan", "satsuma"))))
})

test_that("collapse.dimensions(cat_x_mr_x_mr)", {
    expect_equivalent(as.array(collapse.dimensions(cat_x_mr_x_mr, c(2, 3))),
                      cubify(
                          10000, 10000,
                          dims=cat_x_mr_x_mr_dims["animal"]))
    
    expect_equivalent(as.array(collapse.dimensions(cat_x_mr_x_mr, c(1, 3))),
                      cubify(
                          6970, 7029, 6940,
                          dims=cat_x_mr_x_mr_dims["opinion_mr"]))
    
    expect_equivalent(as.array(collapse.dimensions(cat_x_mr_x_mr, c(1,2))),
                      cubify(
                          3867, 7002,
                          dims=cat_x_mr_x_mr_dims["feeling_mr"]))
})

with_test_authentication({
    ds <- newDatasetFromFixture("apidocs")
    
    test_that("collapse.dimensions(~x+y, 1) == collapse.dimensions(~x)", {
        bivariate_cube <- crtabs(~ allpets + q1, data=ds)
        univariate_allpets <- crtabs(~ allpets, data=ds)
        univariate_q1 <- crtabs(~ q1, data=ds)
        expect_equal(collapse.dimensions(bivariate_cube, 1), univariate_q1)
        expect_equal(collapse.dimensions(bivariate_cube, 2), univariate_allpets)
        
        
        trivariate_cube <- crtabs(~ country + allpets + q1, data=ds)
        expect_equal(
            collapse.dimensions(trivariate_cube, 1),
            crtabs(~ allpets + q1, data=ds))
        expect_equal(
            collapse.dimensions(trivariate_cube, 2),
            crtabs(~ country + q1, data=ds))
        expect_equal(
            collapse.dimensions(trivariate_cube, 3),
            crtabs(~ country + allpets, data=ds))
        
        expect_equal(
            collapse.dimensions(trivariate_cube, c(1, 2)),
            crtabs(~ q1, data=ds))
        expect_equal(
            collapse.dimensions(trivariate_cube, c(2, 3)),
            crtabs(~ country, data=ds))
        expect_equal(
            collapse.dimensions(trivariate_cube, c(1, 3)),
            crtabs(~ allpets, data=ds))
    })
})
