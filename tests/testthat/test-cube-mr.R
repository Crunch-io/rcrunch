context("Multiple response cube specialness")

##########################################
### MR x CAT
##########################################
alldims <- list(
    pdl_gender=c("Male", "Female"),
    attitudes_recoded_klima_2=c("Climate change is the biggest threat to civilisation",
        "Electric cars are the future of the motor industry",
        "I always make an effort to recycle",
        "I always make sure I turn lights off when I leave a room",
        "I don't care what my carbon footprint is",
        "I don't mind paying more for products that are good for the environment")
)

mr_x_cat_wt <- loadCube(test_path("cubes/selected-crosstab-4.json"))

test_that("properties of a cube with for as_selected x cat: it looks 2D", {
    expect_equal(dim(showMissing(mr_x_cat_wt)), c(6L, 3L))
    expect_equal(dim(mr_x_cat_wt), c(6, 2))
    expect_equal(names(dimnames(mr_x_cat_wt)),
        c("attitudes_recoded_klima_2", "pdl_gender"))
    expect_equal(aliases(variables(mr_x_cat_wt)),
        c("attitudes_recoded_klima_2", "pdl_gender"))
    expect_equal(names(categories(variables(mr_x_cat_wt)[[2]])),
        c("Male", "Female", "No Data"))
})

test_that("as.array on cube with as_selected x cat", {
    expect_equal(as.array(mr_x_cat_wt),
        cubify(
                9928.20954289002, 11524.821237084192,
                9588.843313998908, 9801.254016136965,
                11697.435357575358, 13095.670425525452,
                9782.8995547749, 10531.918128023966,
                4417.596222134318, 3448.380316269752,
                6179.175512581436, 6490.427474934746,
            dims=alldims[c("attitudes_recoded_klima_2", "pdl_gender")]))
})

test_that("margin.table for as_selected x cat", {
    expect_equal(margin.table(mr_x_cat_wt, 1),
        cubify(
                21453.03077997421,
                19390.097330135875,
                24793.105783100807,
                20314.817682798865,
                7865.976538404069,
                12669.602987516182,
            dims=alldims["attitudes_recoded_klima_2"]))
    expect_equal(margin.table(mr_x_cat_wt, 2),
        cubify(14566.261567907562, 15607.301233922663,
                 14456.513325488017, 15450.609903833058,
                 14415.136475733132, 15405.898678070093,
                 11485.661204663904, 11912.588886491172,
                 11664.69933815247, 12110.196347286023,
                 11547.413553551738, 11961.575582997419,
            dims=alldims[c("attitudes_recoded_klima_2", "pdl_gender")]))
    expect_error(margin.table(mr_x_cat_wt, 3),
        "Margin 3 exceeds Cube's number of dimensions (2)", fixed=TRUE)
    ## NULL margin is the same shape as margin 1 here but is sum of selected + not
    ## Note: not based on expectation from JS tests
    expect_equal(margin.table(mr_x_cat_wt),
        cubify(
                30173.5628018302,
                29907.1232293211,
                29821.0351538032,
                23398.2500911551,
                23774.8956854385,
                23508.9891365492,
            dims=alldims["attitudes_recoded_klima_2"]))
})

test_that("prop.table(row) for as_selected x cat", {
    expect_equal(prop.table(mr_x_cat_wt, 1),
        cubify(
                0.4627882020361299, 0.5372117979638701,
                0.4945227014975337, 0.5054772985024663,
                0.47180193800279874, 0.5281980619972013,
                0.481564723224583, 0.5184352767754171,
                0.5616081106479636, 0.4383918893520365,
                0.48771658580541166, 0.5122834141945883,
            dims=alldims[c("attitudes_recoded_klima_2", "pdl_gender")]))
})
test_that("prop.table(col) for as_selected x cat", {
    expect_equal(prop.table(mr_x_cat_wt, 2),
        cubify(
                0.6815894041587091, 0.7384249886863752,
                0.6632887957217867, 0.6343603312193796,
                0.8114689290154947, 0.8500426167391849,
                0.8517489224566737, 0.8840998567462627,
                0.3787149667617584, 0.28475015741941767,
                0.535113381358101, 0.5426064007955989,
            dims=alldims[c("attitudes_recoded_klima_2", "pdl_gender")]))
})
test_that("prop.table(cell) for as_selected x cat", {
    ## Note: not based on expectation from JS tests
    expect_equal(prop.table(mr_x_cat_wt),
        cubify(
                0.329036700375595, 0.381950958618156,
                0.320620717695708,  0.327723062528721,
                0.392254504152701, 0.439142047148397,
                0.418103897371069,  0.450115632023491,
                0.185809278853744, 0.14504292098248,
                0.262843097025161,  0.27608279697761,
            dims=alldims[c("attitudes_recoded_klima_2", "pdl_gender")]))
})

##########################################
### CAT x MR
##########################################
cat_x_mr <- loadCube(test_path("cubes/selected-crosstab-array-last.json"))
cat_x_mr_dims <- dimnames(cat_x_mr)
# drop the "No Data" category in animal
cat_x_mr_dims$fruit <- cat_x_mr_dims$fruit[cat_x_mr_dims$fruit != "No Data"]


test_that("margin.table for cat x as_selected", {
    expect_equal(margin.table(cat_x_mr, 1),
                 cubify(
                     49.3701228167475, 48.4562012457907, 56.931621579619,
                     116.632067482127, 93.411479439707, 124.061990989576,
                     dims=cat_x_mr_dims))
    expect_equal(margin.table(cat_x_mr, 2),
                 cubify(22.9672704148528, 45.7789165449064, 86.9728287914322,
                        dims=cat_x_mr_dims["zoo"]))
    ## NULL margin is the same shape as margin 1 here but is sum of selected + not
    expect_equal(margin.table(cat_x_mr),
                 cubify(
                     166.002190298874, 141.867680685498, 180.993612569195,
                     dims=cat_x_mr_dims["zoo"]))
})

test_that("prop.table(row) for cat x as_selected", {
    expect_equal(prop.table(cat_x_mr, 1),
                 cubify(
                     0.17136174576676, 0.355633666090444, 0.463598595422233,
                     0.124383630449479,0.305596916163552,0.488299506632138,
                     dims=cat_x_mr_dims))
})
test_that("prop.table(col) for cat x as_selected", {
    expect_equal(prop.table(cat_x_mr, 2),
                 cubify(
                     0.368356808701398,0.376432161231965,0.303467418114167,
                     0.631643191298602,0.623567838768035,0.696532581885833,
                     dims=cat_x_mr_dims))
})
test_that("prop.table(cell) for cat x as_selected", {
    ## Note: not based on expectation from JS tests
    expect_equal(prop.table(cat_x_mr),
                 cubify(
                     0.0509640892048795,0.121469924725558,0.145825145013507,
                     0.0873911359490897,0.201217500089628,0.334704678977833,
                     dims=cat_x_mr_dims))
})


##########################################
### CAT x MR x MR
##########################################
cat_x_mr_x_mr <- loadCube(test_path("cubes/cat-x-mr-x-mr.json"))
cat_x_mr_x_mr_dims <- dimnames(cat_x_mr_x_mr)
# drop the "No Data" category in animal
cat_x_mr_x_mr_dims$animal <- cat_x_mr_x_mr_dims$animal[cat_x_mr_x_mr_dims$animal != "No Data"]

test_that("prop.table(row) for cat x MR x MR", {
    expect_equal(prop.table(cat_x_mr_x_mr, 1),
                 cubify(0.1159,0.3597,
                        0.0197,0.0604,
                        0.0192,0.0582,
                        0.0159,0.0094,
                        0.1182,0.0625,
                        0.1142,0.0623,
                        dims=cat_x_mr_x_mr_dims))

})
test_that("prop.table(col) for cat x MR x MR", {
    expect_equal(prop.table(cat_x_mr_x_mr, 2),
                 cubify(0.166284074605452,0.516068866571019,
                        0.0280267463366055,0.0859297197325366,
                        0.0276657060518732,0.0838616714697406,
                        0.0228120516499283,0.0134863701578192,
                        0.168160478019633,0.0889173424384692,
                        0.164553314121037,0.0897694524495677,
                        dims=cat_x_mr_x_mr_dims))

})
test_that("prop.table(cell) for cat x MR x MR", {
    expect_equal(prop.table(cat_x_mr_x_mr),
                 cubify(0.05795,0.17985,
                        0.00985,0.0302,
                        0.0096,0.0291,
                        0.00795,0.0047,
                        0.0591,0.03125,
                        0.0571,0.03115,
                        dims=cat_x_mr_x_mr_dims))
})


######################################################################
### Getting an indexable all column MR x MR (from profiles)
######################################################################
mr_x_mr <- loadCube(test_path("cubes/full-cube.json"))
mr_x_mr_dims <- dimnames(mr_x_mr)

test_that("collapse.dimensions(mr_x_mr)", {
    expect_equivalent(as.array(collapse.dimensions(mr_x_mr, 1)),
                      # note this is a 1-d output, condensed here for space
                      cubify(13068.9587689331, 20954.7013096216,
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
                      cubify(19935.9325550077,
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
    # it's the @.Data[[3]] portion of the metadata that's problematic
    mr_x_self <- loadCube(test_path("cubes/natrep-cube.json"))
    expect_equivalent(collapse.dimensions(mr_x_mr, 2),
                      mr_x_self)  
})


test_that("collapse.dimensions(cat_x_mr)", {
    expect_equivalent(as.array(collapse.dimensions(cat_x_mr, 1)),
                      cubify(22.9672704148528, 45.7789165449064, 86.9728287914322,
                             dims=cat_x_mr_dims["zoo"]))
    
    expect_equivalent(as.array(collapse.dimensions(cat_x_mr, 2)),
                      cubify(84.7731562534069, 169.017244048007,
                             dims=list(fruit = list("rambutan", "satsuma"))))
})

test_that("collapse.dimensions(cat_x_mr_x_mr)", {
    expect_equivalent(as.array(collapse.dimensions(cat_x_mr_x_mr, c(2, 3))),
                      cubify(10000, 10000,
                             dims=cat_x_mr_x_mr_dims["animal"]))
    
    expect_equivalent(as.array(collapse.dimensions(cat_x_mr_x_mr, c(1, 3))),
                      cubify(6970, 7029, 6940,
                             dims=cat_x_mr_x_mr_dims["opinion_mr"]))
    
    expect_equivalent(as.array(collapse.dimensions(cat_x_mr_x_mr, c(1,2))),
                      cubify(3867, 7002,
                             dims=cat_x_mr_x_mr_dims["feeling_mr"]))
})

##########################################
### Internal function tests
##########################################
test_that("as_selected_margins adds margins in the right cases", {
    nothing_selected <- c(FALSE, FALSE) # cat by cat
    expect_equal(as_selected_margins(1, nothing_selected), 1)
    expect_equal(as_selected_margins(2, nothing_selected), 2)
    expect_equal(as_selected_margins(NULL, nothing_selected), NULL)
    selecteds <- c(FALSE, TRUE, FALSE) # MR by cat
    expect_equal(as_selected_margins(1, selecteds), 1)
    expect_equal(as_selected_margins(2, selecteds), c(1, 3))
    expect_equal(as_selected_margins(NULL, selecteds), 1)
    selecteds <- c(FALSE, FALSE, TRUE) # cat by MR
    expect_equal(as_selected_margins(1, selecteds), c(1, 2))
    expect_equal(as_selected_margins(2, selecteds), 2)
    expect_equal(as_selected_margins(NULL, selecteds), 2)
    selecteds <- c(FALSE, FALSE, TRUE, FALSE, TRUE) # cat by MR by MR
    expect_equal(as_selected_margins(1, selecteds), c(1, 2, 4))
    expect_equal(as_selected_margins(2, selecteds), c(2, 4))
    expect_equal(as_selected_margins(3, selecteds), c(2, 4))
    expect_equal(as_selected_margins(NULL, selecteds), c(2, 4))
    selecteds <- c(FALSE, TRUE, FALSE, FALSE, TRUE) # MR by cat by MR
    expect_equal(as_selected_margins(1, selecteds), c(1, 4))
    expect_equal(as_selected_margins(2, selecteds), c(1, 3, 4))
    expect_equal(as_selected_margins(3, selecteds), c(1, 4))
    expect_equal(as_selected_margins(NULL, selecteds), c(1, 4))
    selecteds <- c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE) # cat by MR*3
    expect_equal(as_selected_margins(1, selecteds), c(1, 2, 4, 6))
    expect_equal(as_selected_margins(2, selecteds), c(2, 4, 6))
    expect_equal(as_selected_margins(3, selecteds), c(2, 4, 6))
    expect_equal(as_selected_margins(4, selecteds), c(2, 4, 6))
    expect_equal(as_selected_margins(NULL, selecteds), c(2, 4, 6))
})

test_that("as_selected_margins with before=FALSE", {
    nothing_selected <- c(FALSE, FALSE)# cat by cat
    expect_equal(as_selected_margins(1, nothing_selected, before=FALSE), 1)
    expect_equal(as_selected_margins(2, nothing_selected, before=FALSE), 2)
    expect_equal(as_selected_margins(NULL, nothing_selected, before=FALSE), NULL)
    selecteds <- c(FALSE, TRUE, FALSE) # MR by cat
    expect_equal(as_selected_margins(1, selecteds, before=FALSE), 1)
    expect_equal(as_selected_margins(2, selecteds, before=FALSE), c(1, 2))
    expect_equal(as_selected_margins(NULL, selecteds, before=FALSE), 1)
    selecteds <- c(FALSE, FALSE, TRUE) # cat by MR
    expect_equal(as_selected_margins(1, selecteds, before=FALSE), c(1, 2))
    expect_equal(as_selected_margins(2, selecteds, before=FALSE), 2)
    expect_equal(as_selected_margins(NULL, selecteds, before=FALSE), 2)
    selecteds <- c(FALSE, FALSE, TRUE, FALSE, TRUE) # cat by MR by MR
    expect_equal(as_selected_margins(1, selecteds, before=FALSE), c(1, 2, 3))
    expect_equal(as_selected_margins(2, selecteds, before=FALSE), c(2, 3))
    expect_equal(as_selected_margins(3, selecteds, before=FALSE), c(2, 3))
    expect_equal(as_selected_margins(NULL, selecteds, before=FALSE), c(2, 3))
    selecteds <- c(FALSE, TRUE, FALSE, FALSE, TRUE) # MR by cat by MR
    expect_equal(as_selected_margins(1, selecteds, before=FALSE), c(1, 3))
    expect_equal(as_selected_margins(2, selecteds, before=FALSE), c(1, 2, 3))
    expect_equal(as_selected_margins(3, selecteds, before=FALSE), c(1, 3))
    expect_equal(as_selected_margins(NULL, selecteds, before=FALSE), c(1, 3))
    selecteds <- c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE) # cat by MR*3
    expect_equal(as_selected_margins(1, selecteds, before=FALSE), c(1, 2, 3, 4))
    expect_equal(as_selected_margins(2, selecteds, before=FALSE), c(2, 3, 4))
    expect_equal(as_selected_margins(3, selecteds, before=FALSE), c(2, 3, 4))
    expect_equal(as_selected_margins(4, selecteds, before=FALSE), c(2, 3, 4))
    expect_equal(as_selected_margins(NULL, selecteds, before=FALSE), c(2, 3, 4))
})


test_that("user2real translates cube margins with two categoricals", {
    cat_cat <- loadCube(test_path("cubes/cat-x-cat.json"))
    expect_equal(user2real(1, cube = cat_cat), 1)
    expect_equal(user2real(2, cube = cat_cat), 2)
    expect_equal(user2real(c(1, 2), cube = cat_cat), c(1, 2))
    expect_null(user2real(NULL, cube = cat_cat))
})

test_that("user2real translates cube margins with cat by mr by mr", {
    mr_cat <- loadCube(test_path("cubes/selected-crosstab-array-first.json"))
    expect_equal(user2real(1, cube = mr_cat), c(1, 2))
    expect_equal(user2real(2, cube = mr_cat), 3)
    expect_equal(user2real(c(1, 2), cube = mr_cat), c(1, 2, 3))
    expect_null(user2real(NULL, cube = mr_cat))
})

test_that("user2real translates cube margins with cat by mr by mr", {
    cat_mr_mr <- loadCube(test_path("cubes/cat-x-mr-x-mr.json"))
    expect_equal(user2real(1, cube = cat_mr_mr), 1)
    expect_equal(user2real(2, cube = cat_mr_mr), c(2, 3))
    expect_equal(user2real(c(1, 2), cube = cat_mr_mr), c(1, 2, 3))
    expect_equal(user2real(3, cube = cat_mr_mr), c(4, 5))
    expect_null(user2real(NULL, cube = cat_mr_mr))
})

test_that("user2real translates cube margins with catarray by mr", {
    ca_mr <- loadCube(test_path("cubes/catarray-x-mr.json"))
    expect_equal(user2real(1, cube = ca_mr), 1)
    expect_equal(user2real(2, cube = ca_mr), 2)
    expect_equal(user2real(3, cube = ca_mr), c(3, 4))
    expect_equal(user2real(c(1, 2), cube = ca_mr), c(1, 2))
    expect_equal(user2real(3, cube = ca_mr), c(3, 4))
    expect_null(user2real(NULL, cube = ca_mr))   
})


with_mock_crunch({
    ds <- loadDataset("test ds")

    test_that("crtabs use as_selected by default", {
        expect_GET(crtabs(~mymrset, data = ds),
                   "https://app.crunch.io/api/datasets/1/cube/?query=%7B%22dimensions%22%3A%5B%7B%22each%22%3A%22https%3A%2F%2Fapp.crunch.io%2Fapi%2Fdatasets%2F1%2Fvariables%2Fmymrset%2F%22%7D%2C%7B%22function%22%3A%22as_selected%22%2C%22args%22%3A%5B%7B%22variable%22%3A%22https%3A%2F%2Fapp.crunch.io%2Fapi%2Fdatasets%2F1%2Fvariables%2Fmymrset%2F%22%7D%5D%7D%5D%2C%22measures%22%3A%7B%22count%22%3A%7B%22function%22%3A%22cube_count%22%2C%22args%22%3A%5B%5D%7D%7D%2C%22weight%22%3Anull%7D&filter=%7B%7D")

        expect_GET(crtabs(~mymrset+location, data = ds),
                   "https://app.crunch.io/api/datasets/1/cube/?query=%7B%22dimensions%22%3A%5B%7B%22each%22%3A%22https%3A%2F%2Fapp.crunch.io%2Fapi%2Fdatasets%2F1%2Fvariables%2Fmymrset%2F%22%7D%2C%7B%22function%22%3A%22as_selected%22%2C%22args%22%3A%5B%7B%22variable%22%3A%22https%3A%2F%2Fapp.crunch.io%2Fapi%2Fdatasets%2F1%2Fvariables%2Fmymrset%2F%22%7D%5D%7D%2C%7B%22variable%22%3A%22https%3A%2F%2Fapp.crunch.io%2Fapi%2Fdatasets%2F1%2Fvariables%2Flocation%2F%22%7D%5D%2C%22measures%22%3A%7B%22count%22%3A%7B%22function%22%3A%22cube_count%22%2C%22args%22%3A%5B%5D%7D%7D%2C%22weight%22%3Anull%7D&filter=%7B%7D")
    })
})

with_test_authentication({
    ds <- newDatasetFromFixture("apidocs")
    test_that("as_selected in cube query", {
        cub <- as.array(crtabs(~ as_selected(allpets), data=ds))
        expect_identical(dimnames(cub),
            list(
                allpets=c("Cat", "Dog", "Bird")
            ))
    })
    
    test_that("collapse.dimensions(~x+y, 1) == collapse.dimensions(~x)", {
        bivariate_cube <- crtabs(~ allpets + q1, data=ds)
        univariate_allpets <- crtabs(~ allpets, data=ds)
        univariate_q1 <- crtabs(~ q1, data=ds)
        expect_equivalent(collapse.dimensions(bivariate_cube, 1),
                          univariate_q1)
        expect_equivalent(collapse.dimensions(bivariate_cube, 2),
                          univariate_allpets)
        
        
        trivariate_cube <- crtabs(~ country + allpets + q1, data=ds)
        expect_equivalent(collapse.dimensions(trivariate_cube, 1),
                          crtabs(~ allpets + q1, data=ds))
        expect_equivalent(collapse.dimensions(trivariate_cube, 2),
                          crtabs(~ country + q1, data=ds))
        expect_equivalent(collapse.dimensions(trivariate_cube, 3),
                          crtabs(~ country + allpets, data=ds))
        
        expect_equivalent(collapse.dimensions(trivariate_cube, c(1, 2)),
                          crtabs(~ q1, data=ds))
        expect_equivalent(collapse.dimensions(trivariate_cube, c(2, 3)),
                          crtabs(~ country, data=ds))
        expect_equivalent(collapse.dimensions(trivariate_cube, c(1, 3)),
                          crtabs(~ allpets, data=ds))
    })
})
