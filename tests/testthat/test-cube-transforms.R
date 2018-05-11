context("Cube transformations")

unicat_trans_cube <- loadCube(test_path("cubes/univariate-categorical-with-trans.json"))

test_that("Can show a simple cube with transform", {
    loc_array <- array(c(10, 5, 15, NA),
                     dimnames = list("v7" = c("C", "E", "C, E", "D, E")))
    expect_prints(expect_equivalent(showTransforms(unicat_trans_cube), loc_array))
})

test_that("can retrieve transformations from a cube", {
    trans <- list(
        'v7' = Transforms(insertions = Insertions(
            Subtotal(name = c("C, E"), after = 3, categories = c(1, 3)),
            Subtotal(name = c("D, E"), after = 3, categories = c(2, 3))),
            categories = NULL,
            elements = NULL))
    expect_equivalent(transforms(unicat_trans_cube), trans)
})

test_that("can remove transformations from a cube", {
    transforms(unicat_trans_cube) <- NULL
    expect_null(transforms(unicat_trans_cube))

    # alternatively
    expect_null(transforms(noTransforms(unicat_trans_cube)))
})

complex_trans_cube <- loadCube(test_path("cubes/complex-categorical-with-trans.json"))

test_that("Can show a complex cube with transform", {
    # "top" and "bottom" anchors, multiple insertions at the same anchor, an
    # anchor that doesn't exist (and so the insertion should be at the end)
    loc_array <- array(c(40, 10, 20, 30, 30, 40, 50, 60, 70, 250, 250, 80, 90,
                         100, 520, 150, NA),
                     dimnames = list("v7" = c("First!", "A", "B", "Top 2", "C",
                                              "D", "E", "F", "G", "Middle 5",
                                              "Middle 5 (again)", "H", "I", "J",
                                              "Bottom 8", 
                                              "Middle 3 (missing anchor)",
                                              "J and can't see")))
    expect_prints(expect_equivalent(showTransforms(complex_trans_cube), loc_array))
})

pet_feelings <- pet_feelings_headers <- loadCube(test_path("./cubes/feelings-pets.json"))

# add a header for some tests
new_trans <- pet_feelings_headers@dims$feelings$references$view$transform
new_trans$insertions <- c(new_trans$insertions, list(list(name = "Subtitle", anchor = "top")))
pet_feelings_headers@dims$feelings$references$view$transform <- new_trans

test_that("applyTransforms works with a simple cube and row transforms", {
    all <- array(c(9, 12, 21, 12, 10, 11, 21,
                   5, 12, 17, 7, 10, 12, 22),
                 dim = c(7, 2),
                 dimnames = list("feelings" =
                                     c("extremely happy", "somewhat happy",
                                       "happy", "neutral", "somewhat unhappy",
                                       "extremely unhappy", "unhappy"),
                                 "animals" = c("cats", "dogs")))
    expect_equivalent(applyTransforms(pet_feelings), all)

    # can apply to an array of the same shape
    new_array <- cubeToArray(pet_feelings)-1
    new_all <- all - c(1, 1, 2, 1, 1, 1, 2) # must subtract two from every subtotal
    expect_equivalent(applyTransforms(pet_feelings, array = new_array), new_all)
})

test_that("applyTransforms can return what is asked for", {
    all <- array(c(NA, 9, 12, 21, 12, 10, 11, 21,
                   NA, 5, 12, 17, 7, 10, 12, 22),
                 dim = c(8, 2),
                 dimnames = list("feelings" =
                                     c("Subtitle", "extremely happy", "somewhat happy",
                                       "happy", "neutral", "somewhat unhappy",
                                       "extremely unhappy", "unhappy"),
                                 "animals" = c("cats", "dogs")))

    expect_equivalent(applyTransforms(pet_feelings_headers), all)

    pet_array <- cubeToArray(pet_feelings_headers)
    feeling_cats <- Categories(data=index(variables(pet_feelings_headers))[[1]]$categories)
    insert_map <- mapInsertions(transforms(pet_feelings_headers)[[1]]$insertions,
                                feeling_cats,
                                include = c("subtotals", "headings"))
    tst <- apply(pet_array, 2, calcInsertions,
                 insert_map,
                 feeling_cats)
    expect_equivalent(tst, all[c(1, 4, 8),])
})

test_that("applyTransforms with a cube that has transform but no insertions", {
    pet_feelings@dims$feelings$references$view$transform$insertions <- list()

    all <- array(c(9, 12, 12, 10, 11,
                   5, 12, 7, 10, 12),
                 dim = c(5, 2),
                 dimnames = list("feelings" =
                                     c("extremely happy", "somewhat happy",
                                       "neutral", "somewhat unhappy",
                                       "extremely unhappy"),
                                 "animals" = c("cats", "dogs")))
    expect_equivalent(applyTransforms(pet_feelings), all)
})

test_that("categorical arrays with transforms don't error and display cube cells", {
    # TODO: when column display is available, these should be replaced with
    # proper expectations
    cat_array_cube <- loadCube(test_path("./cubes/catarray-with-transforms.json"))

    all <- array(c(1, 2, 2, 2, 1, 1),
                 dim = c(3, 2),
                 dimnames = list("CA" =
                                     c("mr_1", "mr_2", "mr_3"),
                                 "CA" = c("A", "B")))

    expect_equivalent(applyTransforms(cat_array_cube), all)
    expect_prints(cat_array_cube,
                  "    CA\nCA    A  B\nmr_1  1  2\nmr_2  2  1\nmr_3  2  1")
})

test_that("can set transforms on a cube", {
    transforms(pet_feelings) <- NULL
    expect_null(transforms(pet_feelings))
    feelings_trans <- Transforms(
        insertions = Insertions(
            Heading(name = "Fabulous new header", position = "top"),
            Subtotal(name = "moderately happy",
                     after = "somewhat unhappy",
                     categories = c("somewhat happy", "neutral",
                                    "somewhat unhappy"))
        ))
    transforms(pet_feelings) <- list("feelings" = feelings_trans)

    # add empty elements/categories
    feelings_trans["elements"] <- feelings_trans["categories"] <- list(NULL)

    # convert to category ids
    feelings_trans$insertions[["moderately happy"]]$categories <- c(4L, 3L, 5L)
    feelings_trans$insertions[["moderately happy"]]$after <- 5L

    # ensure the transforms were set appropriately
    expect_equal(transforms(pet_feelings),
                 list(feelings = feelings_trans,
                      animals = NULL))

    all <- array(c(NA, 9, 12, 12, 10, 34, 11,
                   NA, 5, 12, 7, 10, 29, 12),
                 dim = c(7, 2),
                 dimnames = list("feelings" =
                                     c("Fabulous new header", "extremely happy",
                                       "somewhat happy", "neutral",
                                       "somewhat unhappy", "moderately happy",
                                       "extremely unhappy"),
                                 "animals" = c("cats", "dogs")))
    expect_equivalent(applyTransforms(pet_feelings), all)

    expect_error(
        transforms(pet_feelings) <- list("not in the var" = Transforms(
            insertions = Insertions(
                Heading(name = "Fabulous new header", position = "top"),
                Subtotal(name = "subtotal", after = 2, categories = c(1, 2))
            ))),
        paste0("The names of the transforms supplied .*not in the var.* do not",
               " match the dimensions of the cube .*feelings.* and .*animals.*")
    )
})

test_that("margin.table works with a simple cube and row transforms", {
    feelings_margin <- array(c(14, 24, 38, 19, 20, 23, 43),
                     dimnames = list("feelings" = c("extremely happy",
                                                    "somewhat happy", "happy",
                                                    "neutral", "somewhat unhappy",
                                                    "extremely unhappy", "unhappy")))
    expect_equivalent(margin.table(pet_feelings, 1), feelings_margin)

    pets_margin <- array(c(54, 46),
                             dimnames = list("animals" = c("cats", "dogs")))
    expect_equivalent(margin.table(pet_feelings, 2), pets_margin)

    expect_equivalent(margin.table(pet_feelings), 100)
})

test_that("prop.table works with a simple cube and row transforms", {
    feelings_prop <- array(c(9/14, 12/24, 21/38, 12/19, 10/20, 11/23, 21/43,
                               5/14, 12/24, 17/38, 7/19, 10/20, 12/23, 22/43),
                             dim = c(7, 2),
                             dimnames = list(
                                 "feelings" =
                                     c("extremely happy", "somewhat happy",
                                       "happy", "neutral", "somewhat unhappy",
                                       "extremely unhappy", "unhappy"),
                                 "animals" = c("cats", "dogs")))
    expect_equivalent(prop.table(pet_feelings, 1), feelings_prop)

    pets_prop <- array(c(9/54, 12/54, 21/54, 12/54, 10/54, 11/54, 21/54,
                         5/46, 12/46, 17/46, 7/46, 10/46, 12/46, 22/46),
                       dim = c(7, 2),
                       dimnames = list(
                           "feelings" =
                               c("extremely happy", "somewhat happy",
                                 "happy", "neutral", "somewhat unhappy",
                                 "extremely unhappy", "unhappy"),
                           "animals" = c("cats", "dogs")))
    expect_equivalent(prop.table(pet_feelings, 2), pets_prop)

    all_prop <- array(c(9/100, 12/100, 21/100, 12/100, 10/100, 11/100, 21/100,
                        5/100, 12/100, 17/100, 7/100, 10/100, 12/100, 22/100),
                      dim = c(7, 2),
                      dimnames = list(
                          "feelings" =
                              c("extremely happy", "somewhat happy",
                                "happy", "neutral", "somewhat unhappy",
                                "extremely unhappy", "unhappy"),
                          "animals" = c("cats", "dogs")))
    expect_equivalent(prop.table(pet_feelings), all_prop)
})

test_that("Can get subtotals alone", {
    subtotes <- array(c(21, 21,
                        17, 22),
                 dim = c(2, 2),
                 dimnames = list("feelings" =
                                     c("happy", "unhappy"),
                                 "animals" = c("cats", "dogs")))
    expect_equivalent(subtotalArray(pet_feelings_headers), subtotes)
})

test_that("Can get subtotals with headers", {
    subtotes <- array(c(NA, 21, 21,
                        NA, 17, 22),
                      dim = c(3, 2),
                      dimnames = list("feelings" =
                                          c("Subtitle", "happy", "unhappy"),
                                      "animals" = c("cats", "dogs")))

    expect_equivalent(subtotalArray(pet_feelings_headers, headings = TRUE), subtotes)
})



cat <- loadCube(test_path("cubes/cat-subtotals-0id.json"))
cat_dims <- dimnames(cat)
# drop no data categories, and add in the subtotals
cat_dims$food_groups <- cat_dims$food_groups[!(cat_dims$food_groups %in% c("Don't know", "No Data", "Not asked"))]
cat_dims_subtotals <- cat_dims
cat_dims_subtotals$food_groups <- c(
    cat_dims_subtotals$food_groups[1], "plant-based",
    cat_dims_subtotals$food_groups[c(2, 3, 4)], "animal-based",
    "plant-based again, after animal-based")

test_that("subtotals with 0 anchor attach to 0 and not top", {
          all <- cubify(
              376.775218800139,
              1180.53898816961,
              485.589210635439,
              318.174558734034,
              471.521308228948,
              471.521308228948,
              1180.53898816961,
              dims = cat_dims_subtotals)
          expect_equivalent(applyTransforms(cat), all)
})


test_that("subtotals with bases on weighted cube", {
    unweighted_counts <- cubify(
        434,
        1223,
        495,
        294,
        433,
        433,
        1223,
        dims = cat_dims_subtotals)
    expect_equivalent(bases(cat, 0), unweighted_counts)
    expect_equivalent(bases(cat), 1656)
})

test_that("cat by mr, where the cat has subtotals works", {
    # cat by mr with subtotals fixture
    cat_mr <- loadCube(test_path("cubes/cat-x-mr-subtotals-on-cat.json"))
    cat_mr_dims <- dimnames(cat_mr)
    # drop no data categories, and add in the subtotals
    cat_mr_dims$food_groups <- cat_mr_dims$food_groups[!(cat_mr_dims$food_groups %in% c("Don't know", "No Data", "Not asked"))]
    cat_mr_dims_subtotals <- cat_mr_dims
    cat_mr_dims_subtotals$food_groups <- c(
    cat_mr_dims_subtotals$food_groups[1], "plant-based",
    cat_mr_dims_subtotals$food_groups[c(2, 3, 4)], "animal-based")

    all <- cubify(
        7.09439811221956, 29.943091432266,  26.594536972556,  104.244359622909, 235.256710642724,
        28.3930651341193, 99.907133775628, 121.487888771867,  399.597650747672, 626.93247871747,
        16.4723263871271, 41.5273628588211, 58.5641962784524, 183.864543659439, 234.846288302351,
        4.82634063477261, 28.4366794845409, 36.3291555208591, 111.488747465324, 156.829479772395,
        12.217223612475,  42.1476791820657, 89.3309048228944, 218.631137785724, 171.129707467715,
        12.217223612475,  42.1476791820657, 89.3309048228944, 218.631137785724, 171.129707467715,
        dims = cat_mr_dims_subtotals)
    expect_equivalent(applyTransforms(cat_mr), all)
    
    # can apply to an array of the same shape
    new_array <- cubeToArray(cat_mr)-1
    # must subtract one for each category in the subtotal
    new_all <- all - c(1, 3, 1, 1, 1, 1) 
    expect_equivalent(applyTransforms(cat_mr, array = new_array), new_all)
    
    # margin.table works with subtotals
    row_margin <- cubify(
        51.911366492838,   69.0306061146165,  70.6657653721693,  142.042366487671, 253.602877279968,
        197.750644752234, 263.820951392254,  276.216370215392,   509.242733468184, 726.557193538396,         
        93.7790931477866, 121.118408249056,  130.06549190286,    231.730645711963, 279.991871527124,         
        52.0601851116097,  73.6719370285819,  75.4851129403625, 135.46972126855,   192.962444731304,         
        70.2849657255216,  94.3678915294494, 135.475226421184,  251.200447977195,  215.124923979429,
        70.2849657255216,  94.3678915294494, 135.475226421184,  251.200447977195,  215.124923979429,
        dims = cat_mr_dims_subtotals)
    expect_equivalent(margin.table(cat_mr, 1), row_margin)
    
    col_margin <- cubify(
        40.6102887465943,
        142.054812957694,
        210.818793594762,
        618.228788533396,
        798.062186185185,
        dims = cat_mr_dims_subtotals['nordics'])
    expect_equivalent(margin.table(cat_mr, 2), col_margin)

    table_margin <- cubify(
        268.035610477756,
        358.188842921703,
        411.691596636576,
        760.44318144538,
        941.682117517826,
        dims = cat_mr_dims_subtotals['nordics'])
    expect_equivalent(margin.table(cat_mr), table_margin)        
    
    # prop.table works with subtotals
    row_prop <- cubify(
        0.136663674865857,  0.433765442860944, 0.376342587283855, 0.733896246595956, 0.927657892394531,
        0.143580139370435,  0.378692947805666, 0.439828706304162, 0.784689941525965, 0.8628811114845,
        0.175650305779438,  0.34286582410684,  0.450266980285526, 0.793440777306508, 0.838761093389815,
        0.0927069434045541, 0.385990658471605, 0.48127576558786,  0.822979086554057, 0.812746127832161,
        0.173824138439307,  0.44663156608636,  0.659389226965898, 0.870345333960438, 0.795489914892794,
        0.173824138439307,  0.44663156608636,  0.659389226965898, 0.870345333960438, 0.795489914892794, 
        dims = cat_mr_dims_subtotals)
    expect_equivalent(prop.table(cat_mr, 1), row_prop)
    
    col_prop <- cubify(
        0.174694599107339, 0.210785476456778, 0.12614879593551,  0.168617769920104, 0.294784936205628,
        0.699159400497994, 0.703299886117777, 0.576266881620586, 0.646358853161181, 0.785568455152935,
        0.405619533756911, 0.292333374661432, 0.27779400156812,  0.297405340983255, 0.294270662571973,
        0.118845267633744, 0.200181034999566, 0.172324084116957, 0.180335742257822, 0.196512856375335,
        0.300840599502006, 0.296700113882224, 0.423733118379414, 0.353641146838819, 0.214431544847065,
        0.300840599502006, 0.296700113882224, 0.423733118379414, 0.353641146838819, 0.214431544847065,
        dims = cat_mr_dims_subtotals)
    expect_equivalent(prop.table(cat_mr, 2), col_prop)
    
    table_prop <- cubify(
        0.0264681177981324, 0.0835958238900571, 0.064598202124666,  0.137083692991725, 0.249826036054328,
        0.105930197422315,  0.278923075773934,  0.295094409903907,  0.525479957605977, 0.66575808020014,
        0.0614557385034259, 0.115937064147748,  0.142252590912489,  0.241786037597137, 0.249390196472437,
        0.0180063411207562, 0.0793901877361293, 0.0882436168667512, 0.146610227017115, 0.166541847673375,
        0.0455805987521531, 0.117668877786008,  0.216985009052181,  0.287504895987324, 0.181727681012776,
        0.0455805987521531, 0.117668877786008,  0.216985009052181,  0.287504895987324, 0.181727681012776,
        dims = cat_mr_dims_subtotals)
    expect_equivalent(prop.table(cat_mr), table_prop) 
})

with_test_authentication({
    df <- data.frame(pets=c(rep("Dogs", 50),
                            rep("Cats", 45),
                            rep("Birds", 30),
                            rep("Lizards", 25),
                            rep("Rocks", 5),
                            rep(NA, 10)))

    ds <- newDataset(df)

    # set rocks to be missing
    is.na(categories(ds$pets)) <- c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)

    insrts_list <- list(list(anchor = "top", name = "First one",
                             `function` = "subtotal", args = c(1, 2)),
                        list(anchor = "bottom", name = "Toward the end",
                             `function` = "subtotal", args = c(3, 4)),
                        list(anchor = 3, name = "Dogs+Cats",
                             `function` = "subtotal", args = c(2, 3)),
                        list(anchor = 4, name = "Birds+Lizards",
                             `function` = "subtotal", args = c(1, 4)),
                        list(anchor = 5, name = "Cats+Birds (missing anch.)",
                             `function` = "subtotal", args = c(2, 1)),
                        list(anchor = "bottom", name = "Rocks+Birds (incl. missing)",
                             `function` = "subtotal", args = c(5, 1)))

    ## test variable-based methods
    test_that("showTransforms before a transform returns a the standard table", {
        cat_summary <- array(c(30, 45, 50, 25),
                             dim = 4,
                             dimnames = list(pets = c("Birds", "Catds",
                                                      "Dogs", "Lizards")))
        expect_prints(expect_equivalent(showTransforms(ds$pets), cat_summary))
    })

    # add transforms
    transforms(ds$pets) <- Transforms(insertions=Insertions(data=insrts_list))

    test_that("summary still works after adding transforms", {
        cat_summary <- array(c(50, 45, 30, 25),
                             dim = c(4,1),
                             dimnames = list(pets = c("Dogs", "Catds",
                                                      "Birds", "Lizards"),
                                             Count = NULL))
        class(cat_summary) <- "CategoricalVariableSummary"

        expect_equivalent(summary(ds$pets), cat_summary)
    })

    test_that("showTransforms works on a variable", {
        cat_show_trans <- array(c(75, 30, 45, 50, 95, 25, 55, 75, 75, NA),
                                dim = 10,
                                dimnames = list(pets = c(
                                 "First one", "Birds", "Cats", "Dogs",
                                 "Dogs+Cats", "Lizards", "Birds+Lizards",
                                 "Toward the end", "Cats+Birds (missing anch.)",
                                 "Rocks+Birds (incl. missing)")))
        expect_prints(trans_pets <- showTransforms(ds$pets),
                      paste(
            "                             ",
            "                           ",
            "\033[30m\033[3m                  First one 75\033[23m\033[39m",
            "                      Birds 30",
            "                       Cats 45",
            "                       Dogs 50",
            "\033[30m\033[3m                  Dogs+Cats 95\033[23m\033[39m",
            "                    Lizards 25",
            "\033[30m\033[3m              Birds+Lizards 55\033[23m\033[39m",
            "\033[30m\033[3m             Toward the end 75\033[23m\033[39m",
            "\033[30m\033[3m Cats+Birds (missing anch.) 75\033[23m\033[39m",
            "\033[30m\033[3mRocks+Birds (incl. missing) NA\033[23m\033[39m",
            sep = "\n"),
            fixed = TRUE)
        expect_is(trans_pets, "array")
        expect_equal(dim(trans_pets), 10)
        expect_equivalent(trans_pets, cat_show_trans)
    })

    test_that("showTransforms works on a variable", {
        cat_show_trans <- array(c(75, 30, 45, 50, 95, 25, 55, 75, 75, NA),
                                dimnames = list(pets = c(
                                    "First one", "Birds", "Cats", "Dogs",
                                    "Dogs+Cats", "Lizards", "Birds+Lizards",
                                    "Toward the end", "Cats+Birds (missing anch.)",
                                    "Rocks+Birds (incl. missing)")))

        pets_cube <- crtabs(~pets, ds)
        expect_prints(trans_cube <- showTransforms(pets_cube),
                      paste(
              "                             ",
              "                           ",
              "\033[30m\033[3m                  First one 75\033[23m\033[39m",
              "                      Birds 30",
              "                       Cats 45",
              "                       Dogs 50",
              "\033[30m\033[3m                  Dogs+Cats 95\033[23m\033[39m",
              "                    Lizards 25",
              "\033[30m\033[3m              Birds+Lizards 55\033[23m\033[39m",
              "\033[30m\033[3m             Toward the end 75\033[23m\033[39m",
              "\033[30m\033[3m Cats+Birds (missing anch.) 75\033[23m\033[39m",
              "\033[30m\033[3mRocks+Birds (incl. missing) NA\033[23m\033[39m",
              sep = "\n"),
              fixed = TRUE)
        expect_is(trans_cube, "array")
        expect_equal(dim(showMissing(pets_cube)), 6)
        expect_equal(dim(trans_cube), 10)
        expect_equivalent(trans_cube, cat_show_trans)
    })
})
