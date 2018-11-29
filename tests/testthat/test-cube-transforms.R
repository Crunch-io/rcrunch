context("Cube transformations")

##############################################################
### Transforms calculation tests (ie the numbers are right)
##############################################################

unicat_trans_cube <- loadCube("cubes/univariate-categorical-with-trans.json")

test_that("Can show a simple cube with transform", {
    loc_array <- cubify(c(10, 5, 15, 5),
        dims = list("v7" = c("C", "E", "C, E", "D, E"))
    )
    expect_prints(expect_equivalent(showTransforms(unicat_trans_cube), loc_array))
})

complex_trans_cube <- loadCube("cubes/complex-categorical-with-trans.json")

test_that("Can show a complex cube with transform", {
    # "top" and "bottom" anchors, multiple insertions at the same anchor, an
    # anchor that doesn't exist (and so the insertion should be at the end)
    loc_array <- cubify(
        c(
            40, 10, 20, 30, 30, 40, 50, 60, 70, 250, 250, 80, 90,
            100, 520, 150, 100
        ),
        dims = list("v7" = c(
            "First!", "A", "B", "Top 2", "C",
            "D", "E", "F", "G", "Middle 5",
            "Middle 5 (again)", "H", "I", "J",
            "Bottom 8",
            "Middle 3 (missing anchor)",
            "J and can't see"
        ))
    )
    expect_prints(expect_equivalent(showTransforms(complex_trans_cube), loc_array))
})

pet_feelings <- loadCube("./cubes/feelings-pets.json")

pet_feelings_headers <- pet_feeling_both <- pet_feelings

# add a header for some tests
new_trans <- pet_feelings_headers@dims$feelings$references$view$transform
new_trans$insertions <- c(new_trans$insertions, list(list(name = "Subtitle", anchor = "top")))
pet_feelings_headers@dims$feelings$references$view$transform <- new_trans

test_that("simple with row subtotals", {
    all <- cubify(
        c(
            9, 5,
            12, 12,
            21, 17,
            12, 7,
            10, 10,
            11, 12,
            21, 22
        ),
        dims = list(
            "feelings" =
                c(
                    "extremely happy", "somewhat happy",
                    "happy", "neutral", "somewhat unhappy",
                    "extremely unhappy", "unhappy"
                ),
            "animals" = c("cats", "dogs")
        )
    )
    expect_equivalent(applyTransforms(pet_feelings), all)

    # can apply to an array of the same shape
    new_array <- cubeToArray(pet_feelings) - 1
    new_all <- all - c(1, 1, 2, 1, 1, 1, 2) # must subtract two from every subtotal
    expect_equivalent(applyTransforms(pet_feelings, array = new_array), new_all)
})

test_that("simple with row subtotals (margins and proportions)", {
    feelings_margin <- cubify(
        c(14, 24, 38, 19, 20, 23, 43),
        dims = list("feelings" = c(
            "extremely happy",
            "somewhat happy", "happy",
            "neutral", "somewhat unhappy",
            "extremely unhappy", "unhappy"
        ))
    )
    expect_equivalent(margin.table(pet_feelings, 1), feelings_margin)

    pets_margin <- cubify(c(54, 46),
        dims = list("animals" = c("cats", "dogs"))
    )
    expect_equivalent(margin.table(pet_feelings, 2), pets_margin)

    expect_equivalent(margin.table(pet_feelings), 100)

    feelings_prop <- cubify(
        c(
            9 / 14, 5 / 14,
            12 / 24, 12 / 24,
            21 / 38, 17 / 38,
            12 / 19, 7 / 19,
            10 / 20, 10 / 20,
            11 / 23, 12 / 23,
            21 / 43, 22 / 43
        ),
        dims = list(
            "feelings" =
                c(
                    "extremely happy", "somewhat happy",
                    "happy", "neutral", "somewhat unhappy",
                    "extremely unhappy", "unhappy"
                ),
            "animals" = c("cats", "dogs")
        )
    )
    expect_equivalent(prop.table(pet_feelings, 1), feelings_prop)

    pets_prop <- cubify(
        c(
            9 / 54, 5 / 46,
            12 / 54, 12 / 46,
            21 / 54, 17 / 46,
            12 / 54, 7 / 46,
            10 / 54, 10 / 46,
            11 / 54, 12 / 46,
            21 / 54, 22 / 46
        ),
        dims = list(
            "feelings" =
                c(
                    "extremely happy", "somewhat happy",
                    "happy", "neutral", "somewhat unhappy",
                    "extremely unhappy", "unhappy"
                ),
            "animals" = c("cats", "dogs")
        )
    )
    expect_equivalent(prop.table(pet_feelings, 2), pets_prop)

    all_prop <- cubify(
        c(
            9 / 100, 5 / 100,
            12 / 100, 12 / 100,
            21 / 100, 17 / 100,
            12 / 100, 7 / 100,
            10 / 100, 10 / 100,
            11 / 100, 12 / 100,
            21 / 100, 22 / 100
        ),
        dims = list(
            "feelings" =
                c(
                    "extremely happy", "somewhat happy",
                    "happy", "neutral", "somewhat unhappy",
                    "extremely unhappy", "unhappy"
                ),
            "animals" = c("cats", "dogs")
        )
    )
    expect_equivalent(prop.table(pet_feelings), all_prop)
})

test_that("applyTransforms can return what is asked for", {
    all <- cubify(
        c(
            NA, NA,
            9, 5,
            12, 12,
            21, 17,
            12, 7,
            10, 10,
            11, 12,
            21, 22
        ),
        dims = list(
            "feelings" =
                c(
                    "Subtitle", "extremely happy", "somewhat happy",
                    "happy", "neutral", "somewhat unhappy",
                    "extremely unhappy", "unhappy"
                ),
            "animals" = c("cats", "dogs")
        )
    )

    expect_equivalent(applyTransforms(pet_feelings_headers), all)

    pet_array <- cubeToArray(pet_feelings_headers)
    insert_funcs <-  makeInsertionFunctions(
        Categories(data = index(variables(pet_feelings_headers))[[1]]$categories),
        transforms(pet_feelings_headers)[[1]],
        include = c("subtotals", "headings")
    )

    tst <- apply(pet_array, 2, calcInsertions, insert_funcs)

    expect_equivalent(tst, all[c(1, 4, 8), ])
})

test_that("applyTransforms with a cube that has transform but no insertions", {
    pet_feelings@dims$feelings$references$view$transform$insertions <- list()

    all <- cubify(
        c(
            9, 5,
            12, 12,
            12, 7,
            10, 10,
            11, 12
        ),
        dims = list(
            "feelings" =
                c(
                    "extremely happy", "somewhat happy",
                    "neutral", "somewhat unhappy",
                    "extremely unhappy"
                ),
            "animals" = c("cats", "dogs")
        )
    )
    expect_equivalent(applyTransforms(pet_feelings), all)
})

test_that("applyTransforms handles useNA", {
    # change neutral to missing
    # TODO: setter methods for variables(cube_object)
    pet_feelings@dims$feelings$references$categories[[3]]$missing <- TRUE
    pet_feelings@dims@.Data[[1]]$missing[[3]] <- TRUE
    pet_feelings@.Data[[3]]$dimensions[[1]]$type$categories[[3]]$missing <- TRUE

    all_no <- cubify(
        c(
            9, 5,
            12, 12,
            21, 17,
            10, 10,
            11, 12,
            21, 22
        ),
        dims = list(
            "feelings" =
                c(
                    "extremely happy", "somewhat happy",
                    "happy", "somewhat unhappy",
                    "extremely unhappy", "unhappy"
                ),
            "animals" = c("cats", "dogs")
        )
    )
    # expect silent to catch any warnings that are not raised because of `try`
    expect_silent(
        expect_equivalent(applyTransforms(pet_feelings), all_no)
    )

    all_ifany <- cubify(
        c(
            9, 5,
            12, 12,
            21, 17,
            12, 7,
            10, 10,
            11, 12,
            21, 22
        ),
        dims = list(
            "feelings" =
                c(
                    "extremely happy", "somewhat happy",
                    "happy", "neutral", "somewhat unhappy",
                    "extremely unhappy", "unhappy"
                ),
            "animals" = c("cats", "dogs")
        )
    )

    pet_feelings@useNA <- "ifany"
    # expect silent to catch any warnings that are not raised because of `try`
    expect_silent(
        expect_equivalent(applyTransforms(pet_feelings), all_ifany)
    )

    all_always <- cubify(
        c(
            9, 5, 0,
            12, 12, 0,
            21, 17, 0,
            12, 7, 0,
            10, 10, 0,
            11, 12, 0,
            21, 22, 0,
            0, 0, 0
        ),
        dims = list(
            "feelings" =
                c(
                    "extremely happy", "somewhat happy",
                    "happy", "neutral", "somewhat unhappy",
                    "extremely unhappy", "unhappy", "No Data"
                ),
            "animals" = c("cats", "dogs", "No Data")
        )
    )

    pet_feelings@useNA <- "always"
    # expect silent to catch any warnings that are not raised because of `try`
    expect_silent(
        expect_equivalent(applyTransforms(pet_feelings), all_always)
    )
})




# cat by mr with subtotals fixture
cat_by_cat <- loadCube("cubes/cat-by-cat-col-subtotals.json")
cat_by_cat_dims <- dimnames(cat_by_cat)
# drop no data categories, and add in the subtotals
cat_by_cat_dims$food_groups <- cat_by_cat_dims$food_groups[!(cat_by_cat_dims$food_groups %in% c("Don't know", "No Data", "Not asked"))]
cat_by_cat_dims$offal <- cat_by_cat_dims$offal[!(cat_by_cat_dims$offal %in% c("Don't know", "No Data", "Not asked"))]
cat_by_cat_dims_subtotals <- cat_by_cat_dims
cat_by_cat_dims_subtotals$food_groups <- c(
    cat_by_cat_dims_subtotals$food_groups[1], "plant-based",
    cat_by_cat_dims_subtotals$food_groups[c(2, 3, 4)],
    "animal-based", "plant-based again, after animal-based"
)

test_that("cat by cat with column subtotals", {
    all <- cubify(
        # Vegetables      plant-based       Fruit             Grain
        # Meat            animal-based      plant-based (again)
        115.83196131542, 364.191867955033, 153.417539695033, 94.9423669445809, # Liver
        136.201841877585, 136.201841877585, 364.191867955033,
        37.5716937475242, 127.869673293253, 48.6141816507989, 41.6837978949296, # Kidney
        34.3372374619018, 34.3372374619018, 127.869673293253,
        0, 0, 0, 0, 0, 0, 0, # Heart
        29.9055007459322, 129.671056693162, 70.2237662859661, 29.5417896612636, # Pancreas
        47.0182342996631, 47.0182342996631, 129.671056693162,
        66.5763886292179, 205.347046675315, 73.0511594850247, 65.7194985610729, # Thymus
        154.487897653589, 154.487897653589, 205.347046675315,
        56.9451510924633, 153.827966744574, 62.589746073575, 34.2930695785354, # Snout
        43.7522602181583, 43.7522602181583, 153.827966744574,
        33.845719287749, 98.0351667619131, 32.5062091544317, 31.6832383197324, # Lung
        30.0702030313596, 30.0702030313596, 98.0351667619131,
        6.05492419458171, 19.2847236651074, 7.74346718665446, 5.48633228387126, # Tongue
        11.4481940114737, 11.4481940114737, 19.2847236651074,
        dims = cat_by_cat_dims_subtotals
    )

    expect_equivalent(applyTransforms(cat_by_cat), all)

    # pretty printing tests are large and mostly unreadable for this cube, but
    # column pretty printing is covered by categorical array tests and row+col
    # subtotals test
})

test_that("cat by cat with column subtotals (margins and proportions)", {
    row_margin <- cubify(
        500.393709832618, # Liver
        162.206910755154, # Kidney
        0, # Heart
        176.689290992825, # Pancreas
        359.834944328904, # Thymus
        197.580226962732, # Snout
        128.105369793273, # Lung
        30.7329176765812, # Tongue
        dims = cat_by_cat_dims_subtotals["offal"]
    )
    expect_equivalent(margin.table(cat_by_cat, 1), row_margin)

    col_margin <- cubify(
        346.731339012888, # Vegetables
        1098.22750178836, # plant-based
        448.146069531484, # Fruit
        303.350093243986, # Grain
        457.31586855373, # Meat
        457.31586855373, # Meat
        1098.22750178836, # plant-based (again)
        dims = cat_by_cat_dims_subtotals["food_groups"]
    )
    expect_equivalent(margin.table(cat_by_cat, 2), col_margin)

    expect_equivalent(margin.table(cat_by_cat), 1555.54337034209)
    expect_equivalent(
        margin.table(cat_by_cat),
        margin.table(noTransforms(cat_by_cat))
    )

    row_prop <- cubify(
        # Vegetables        plant-based         Fruit               Grain
        # Meat              animal-based        plant-based (again)
        0.231481649427938, 0.727810643496809, 0.306593661511755, 0.189735332557116, # Liver
        0.272189356503191, 0.272189356503191, 0.727810643496809,
        0.231628193722506, 0.788312117516789, 0.299704750090335, 0.256979173703948, # Kidney
        0.211687882483211, 0.211687882483211, 0.788312117516789,
        NA, NA, NA, NA, NA, NA, NA, # Heart
        0.169254744177714, 0.733893129371534, 0.397442119391479, 0.167196265802341, # Pancreas
        0.266106870628466, 0.266106870628466, 0.733893129371534,
        0.185019241956569, 0.570670108369519, 0.203012966462348, 0.182637899950602, # Thymus
        0.429329891630481, 0.429329891630481, 0.570670108369519,
        0.288212803314597, 0.778559520399726, 0.316781426136234, 0.173565290948895, # Snout
        0.221440479600274, 0.221440479600274, 0.778559520399726,
        0.264202190293559, 0.765269769098011, 0.25374587503153, 0.247321703772922, # Lung
        0.234730230901989, 0.234730230901989, 0.765269769098011,
        0.197017551613579, 0.627494072253433, 0.251960040636007, 0.178516480003846, # Tongue
        0.372505927746567, 0.372505927746567, 0.627494072253433,
        dims = cat_by_cat_dims_subtotals
    )
    expect_equivalent(prop.table(cat_by_cat, 1), row_prop)

    col_prop <- cubify(
        # Vegetables        plant-based         Fruit               Grain
        # Meat              animal-based        plant-based (again)
        0.334068335574115, 0.331617872764962, 0.342338246669047, 0.312979521216821, # Liver
        0.297828812081955, 0.297828812081955, 0.331617872764962,
        0.108359670788592, 0.116432772886337, 0.108478429146155, 0.13741152161573, # Kidney
        0.0750842903625757, 0.0750842903625757, 0.116432772886337,
        0, 0, 0, 0, 0, 0, 0, # Heart
        0.0862497772225333, 0.118073037218614, 0.156698387111555, 0.0973851345992599, # Pancreas
        0.102813476489148, 0.102813476489148, 0.118073037218614,
        0.192011454224919, 0.186980426497176, 0.163007475579104, 0.216645717356725, # Thymus
        0.33781442603809, 0.33781442603809, 0.186980426497176,
        0.16423422023109, 0.140069308493987, 0.13966371754417, 0.113047829363788, # Snout
        0.0956718610192244, 0.0956718610192244, 0.140069308493987,
        0.0976136722573294, 0.0892667198756835, 0.0725348527287441, 0.104444465406013, # Lung
        0.0657536838300782, 0.0657536838300782, 0.0892667198756835,
        0.0174628697014222, 0.0175598622632416, 0.0172788912212259, 0.0180858104416621, # Tongue
        0.0250334501789297, 0.0250334501789297, 0.0175598622632416,
        dims = cat_by_cat_dims_subtotals
    )
    expect_equivalent(prop.table(cat_by_cat, 2), col_prop)

    table_prop <- cubify(
        0.0744639870053551, 0.234125177670194, 0.0986263338072625, 0.0610348568575762,
        0.0875590128018301, 0.0875590128018301, 0.234125177670194,
        0.0241534208970731, 0.0822025767530559, 0.0312522187279856, 0.0267969371279971,
        0.0220741112826385, 0.0220741112826385, 0.0822025767530559,
        0, 0, 0, 0, 0, 0, 0,
        0.0192251153623287, 0.0833606180100561, 0.0451442033856779, 0.0189912992620495,
        0.0302262445368676, 0.0302262445368676, 0.0833606180100561,
        0.0427994422390015, 0.132009849799403, 0.0469618275374473, 0.0422485800229538,
        0.0993144264564057, 0.0993144264564057, 0.132009849799403,
        0.036607883893292, 0.0988901818344959, 0.0402365805202915, 0.0220457174209124,
        0.0281266733235066, 0.0281266733235066, 0.0988901818344959,
        0.02175813283837, 0.0630231008861898, 0.0208970124357787, 0.0203679556120411,
        0.0193309962323626, 0.0193309962323626, 0.0630231008861898,
        0.00389248175912327, 0.0123974194694851, 0.00497798218570502, 0.00352695552465678,
        0.0073596109435098, 0.0073596109435098, 0.0123974194694851,
        dims = cat_by_cat_dims_subtotals
    )
    expect_equivalent(prop.table(cat_by_cat), table_prop)
})

transforms(pet_feeling_both) <- list(
    animals = Transforms(
        insertions = Insertions(
            Subtotal("felines", categories = "cats", after = "cats"),
            Subtotal("both", categories = c("cats", "dogs"), after = "dogs")
        )
    )
)

test_that("cat by cat with both column and row subtotals", {
    all <- cubify(
        c(
            9, 9, 5, 14,
            12, 12, 12, 24,
            21, 21, 17, 38,
            12, 12, 7, 19,
            10, 10, 10, 20,
            11, 11, 12, 23,
            21, 21, 22, 43
        ),
        dims = list(
            "feelings" =
                c(
                    "extremely happy", "somewhat happy", "happy", "neutral",
                    "somewhat unhappy", "extremely unhappy", "unhappy"
                ),
            "animals" = c("cats", "felines", "dogs", "both")
        )
    )

    expect_equivalent(applyTransforms(pet_feeling_both), all)

    # pretty printing
    skip_on_local_env("Pretty formatting isn't exactly the same in many terminals")
    expect_prints(
        pet_feeling_both,
        paste(
            "                 animals",
            "feelings             cats \033[30m\033[3mfelines\033[23m\033[39m    dogs \033[30m\033[3m   both\033[23m\033[39m",
            "  extremely happy       9 \033[30m\033[3m      9\033[23m\033[39m       5 \033[30m\033[3m     14\033[23m\033[39m",
            "   somewhat happy      12 \033[30m\033[3m     12\033[23m\033[39m      12 \033[30m\033[3m     24\033[23m\033[39m",
            "\033[30m\033[3m            happy      21 \033[30m\033[3m     21\033[3m\033[30m      17 \033[30m\033[3m     38\033[3m\033[30m\033[23m\033[39m",
            "          neutral      12 \033[30m\033[3m     12\033[23m\033[39m       7 \033[30m\033[3m     19\033[23m\033[39m",
            " somewhat unhappy      10 \033[30m\033[3m     10\033[23m\033[39m      10 \033[30m\033[3m     20\033[23m\033[39m",
            "extremely unhappy      11 \033[30m\033[3m     11\033[23m\033[39m      12 \033[30m\033[3m     23\033[23m\033[39m",
            "\033[30m\033[3m          unhappy      21 \033[30m\033[3m     21\033[3m\033[30m      22 \033[30m\033[3m     43\033[3m\033[30m\033[23m\033[39m",
            sep = "\n"
        ),
        fixed = TRUE
    )
})

test_that("cat by cat with both column and row subtotals (margins and proportions)", {
    feelings_margin <- cubify(
        c(14, 24, 38, 19, 20, 23, 43),
        dims = list("feelings" = c(
            "extremely happy",
            "somewhat happy", "happy",
            "neutral", "somewhat unhappy",
            "extremely unhappy", "unhappy"
        ))
    )
    expect_equivalent(margin.table(pet_feeling_both, 1), feelings_margin)

    pets_margin <- cubify(
        c(54, 54, 46, 100),
        dims = list("animals" = c("cats", "felines", "dogs", "both"))
    )
    expect_equivalent(margin.table(pet_feeling_both, 2), pets_margin)

    expect_equivalent(margin.table(pet_feeling_both), 100)

    feelings_prop <- cubify(
        c(
            9 / 14, 9 / 14, 5 / 14, 14 / 14,
            12 / 24, 12 / 24, 12 / 24, 24 / 24,
            21 / 38, 21 / 38, 17 / 38, 38 / 38,
            12 / 19, 12 / 19, 7 / 19, 19 / 19,
            10 / 20, 10 / 20, 10 / 20, 20 / 20,
            11 / 23, 11 / 23, 12 / 23, 23 / 23,
            21 / 43, 21 / 43, 22 / 43, 43 / 43
        ),
        dims = list(
            "feelings" =
                c(
                    "extremely happy", "somewhat happy",
                    "happy", "neutral", "somewhat unhappy",
                    "extremely unhappy", "unhappy"
                ),
            "animals" = c("cats", "felines", "dogs", "both")
        )
    )
    expect_equivalent(prop.table(pet_feeling_both, 1), feelings_prop)

    pets_prop <- cubify(
        c(
            9 / 54, 9 / 54, 5 / 46, 14 / 100,
            12 / 54, 12 / 54, 12 / 46, 24 / 100,
            21 / 54, 21 / 54, 17 / 46, 38 / 100,
            12 / 54, 12 / 54, 7 / 46, 19 / 100,
            10 / 54, 10 / 54, 10 / 46, 20 / 100,
            11 / 54, 11 / 54, 12 / 46, 23 / 100,
            21 / 54, 21 / 54, 22 / 46, 43 / 100
        ),
        dims = list(
            "feelings" =
                c(
                    "extremely happy", "somewhat happy",
                    "happy", "neutral", "somewhat unhappy",
                    "extremely unhappy", "unhappy"
                ),
            "animals" = c("cats", "felines", "dogs", "both")
        )
    )
    expect_equivalent(prop.table(pet_feeling_both, 2), pets_prop)

    all_prop <- cubify(
        c(
            9 / 100, 9 / 100, 5 / 100, 14 / 100,
            12 / 100, 12 / 100, 12 / 100, 24 / 100,
            21 / 100, 21 / 100, 17 / 100, 38 / 100,
            12 / 100, 12 / 100, 7 / 100, 19 / 100,
            10 / 100, 10 / 100, 10 / 100, 20 / 100,
            11 / 100, 11 / 100, 12 / 100, 23 / 100,
            21 / 100, 21 / 100, 22 / 100, 43 / 100
        ),
        dims = list(
            "feelings" =
                c(
                    "extremely happy", "somewhat happy",
                    "happy", "neutral", "somewhat unhappy",
                    "extremely unhappy", "unhappy"
                ),
            "animals" = c("cats", "felines", "dogs", "both")
        )
    )
    expect_equivalent(prop.table(pet_feeling_both), all_prop)
})

pet_feeling_bad_feelings <- pet_feeling_bad_animals <- pet_feeling_both
test_that("broken row transforms don't break columns", {
    only_feelings <- cubify(
        c(
            9, 9, 5, 14,
            12, 12, 12, 24,
            12, 12, 7, 19,
            10, 10, 10, 20,
            11, 11, 12, 23
        ),
        dims = list(
            "feelings" =
                c(
                    "extremely happy", "somewhat happy", "neutral",
                    "somewhat unhappy", "extremely unhappy"
                ),
            "animals" = c("cats", "felines", "dogs", "both")
        )
    )

    # malform the transform for animals only
    pet_feeling_bad_feelings@dims$feelings$references$view$transform$insertions[[2]]$anchor <- NA
    expect_warning(
        expect_equivalent(applyTransforms(pet_feeling_bad_feelings), only_feelings),
        "Transforms for dimensions 1 were malformed and have been ignored."
    )

    # pretty printing
    skip_on_local_env("Pretty formatting isn't exactly the same in many terminals")
    expect_warning(expect_prints(
        pet_feeling_bad_feelings,
        paste(
            "                 animals",
            "feelings             cats \033[30m\033[3mfelines\033[23m\033[39m    dogs \033[30m\033[3m   both\033[23m\033[39m",
            "  extremely happy       9 \033[30m\033[3m      9\033[23m\033[39m       5 \033[30m\033[3m     14\033[23m\033[39m",
            "   somewhat happy      12 \033[30m\033[3m     12\033[23m\033[39m      12 \033[30m\033[3m     24\033[23m\033[39m",
            "          neutral      12 \033[30m\033[3m     12\033[23m\033[39m       7 \033[30m\033[3m     19\033[23m\033[39m",
            " somewhat unhappy      10 \033[30m\033[3m     10\033[23m\033[39m      10 \033[30m\033[3m     20\033[23m\033[39m",
            "extremely unhappy      11 \033[30m\033[3m     11\033[23m\033[39m      12 \033[30m\033[3m     23\033[23m\033[39m",
            sep = "\n"
        ),
        fixed = TRUE
    ), "Transforms for dimensions 1 were malformed and have been ignored.")
})

test_that("broken column transforms don't break rows", {
    only_animals <- cubify(
        c(
            9, 5,
            12, 12,
            21, 17,
            12, 7,
            10, 10,
            11, 12,
            21, 22
        ),
        dims = list(
            "feelings" =
                c(
                    "extremely happy", "somewhat happy", "happy", "neutral",
                    "somewhat unhappy", "extremely unhappy", "unhappy"
                ),
            "animals" = c("cats", "dogs")
        )
    )

    # malform the transform for animals only
    pet_feeling_bad_animals@dims$animals$references$view$transform$insertions[[2]]$anchor <- NA
    expect_warning(
        expect_equivalent(applyTransforms(pet_feeling_bad_animals), only_animals),
        "Transforms for dimension(s) 2 were malformed and have been ignored."
    )

    # pretty printing
    skip_on_local_env("Pretty formatting isn't exactly the same in many terminals")
    expect_warning(expect_prints(
        pet_feeling_bad_animals,
        paste(
            "                 animals",
            "feelings             cats    dogs",
            "  extremely happy       9       5",
            "   somewhat happy      12      12",
            "\033[30m\033[3m            happy      21      17\033[23m\033[39m",
            "          neutral      12       7",
            " somewhat unhappy      10      10",
            "extremely unhappy      11      12",
            "\033[30m\033[3m          unhappy      21      22\033[23m\033[39m",
            sep = "\n"
        ),
        fixed = TRUE
    ), "Transforms for dimension(s) 2 were malformed and have been ignored.")
})

test_that("Two bad transforms are both ignored", {
    only_cube <- cubify(
        c(
            9, 5,
            12, 12,
            12, 7,
            10, 10,
            11, 12
        ),
        dims = list(
            "feelings" =
                c(
                    "extremely happy", "somewhat happy", "neutral",
                    "somewhat unhappy", "extremely unhappy"
                ),
            "animals" = c("cats", "dogs")
        )
    )

    # malform the transform for both
    pet_feeling_both@dims$feelings$references$view$transform$insertions[[2]]$anchor <- NA
    pet_feeling_both@dims$animals$references$view$transform$insertions[[2]]$anchor <- NA
    expect_warning(
        expect_equivalent(applyTransforms(pet_feeling_both), only_cube),
        "Transforms for dimension(s) 1 and 2 were malformed and have been ignored."
    )
})


# cat by mr with subtotals fixture
cat_mr <- loadCube("cubes/cat-x-mr-subtotals-on-cat.json")
cat_mr_dims <- dimnames(cat_mr)
# drop no data categories, and add in the subtotals
cat_mr_dims$food_groups <- cat_mr_dims$food_groups[!(cat_mr_dims$food_groups %in% c("Don't know", "No Data", "Not asked"))]
cat_mr_dims_subtotals <- cat_mr_dims
cat_mr_dims_subtotals$food_groups <- c(
    cat_mr_dims_subtotals$food_groups[1], "plant-based",
    cat_mr_dims_subtotals$food_groups[c(2, 3, 4)], "animal-based"
)

test_that("cat by mr, with cat subtotals", {
    all <- cubify(
        7.09439811221956, 29.943091432266, 26.594536972556, 104.244359622909, 235.256710642724,
        28.3930651341193, 99.907133775628, 121.487888771867, 399.597650747672, 626.93247871747,
        16.4723263871271, 41.5273628588211, 58.5641962784524, 183.864543659439, 234.846288302351,
        4.82634063477261, 28.4366794845409, 36.3291555208591, 111.488747465324, 156.829479772395,
        12.217223612475, 42.1476791820657, 89.3309048228944, 218.631137785724, 171.129707467715,
        12.217223612475, 42.1476791820657, 89.3309048228944, 218.631137785724, 171.129707467715,
        dims = cat_mr_dims_subtotals
    )
    expect_equivalent(applyTransforms(cat_mr), all)

    # can apply to an array of the same shape
    new_array <- cubeToArray(cat_mr) - 1
    # must subtract one for each category in the subtotal
    new_all <- all - c(1, 3, 1, 1, 1, 1)
    expect_equivalent(applyTransforms(cat_mr, array = new_array), new_all)
})

test_that("cat by mr, with cat subtotals (margins and proportions)", {
    row_margin <- cubify(
        51.911366492838, 69.0306061146165, 70.6657653721693, 142.042366487671, 253.602877279968,
        197.750644752234, 263.820951392254, 276.216370215392, 509.242733468184, 726.557193538396,
        93.7790931477866, 121.118408249056, 130.06549190286, 231.730645711963, 279.991871527124,
        52.0601851116097, 73.6719370285819, 75.4851129403625, 135.46972126855, 192.962444731304,
        70.2849657255216, 94.3678915294494, 135.475226421184, 251.200447977195, 215.124923979429,
        70.2849657255216, 94.3678915294494, 135.475226421184, 251.200447977195, 215.124923979429,
        dims = cat_mr_dims_subtotals
    )
    expect_equivalent(margin.table(cat_mr, 1), row_margin)

    col_margin <- cubify(
        40.6102887465943,
        142.054812957694,
        210.818793594762,
        618.228788533396,
        798.062186185185,
        dims = cat_mr_dims_subtotals["nordics"]
    )
    expect_equivalent(margin.table(cat_mr, 2), col_margin)

    table_margin <- cubify(
        268.035610477756,
        358.188842921703,
        411.691596636576,
        760.44318144538,
        941.682117517826,
        dims = cat_mr_dims_subtotals["nordics"]
    )
    expect_equivalent(margin.table(cat_mr), table_margin)

    row_prop <- cubify(
        0.136663674865857, 0.433765442860944, 0.376342587283855, 0.733896246595956, 0.927657892394531,
        0.143580139370435, 0.378692947805666, 0.439828706304162, 0.784689941525965, 0.8628811114845,
        0.175650305779438, 0.34286582410684, 0.450266980285526, 0.793440777306508, 0.838761093389815,
        0.0927069434045541, 0.385990658471605, 0.48127576558786, 0.822979086554057, 0.812746127832161,
        0.173824138439307, 0.44663156608636, 0.659389226965898, 0.870345333960438, 0.795489914892794,
        0.173824138439307, 0.44663156608636, 0.659389226965898, 0.870345333960438, 0.795489914892794,
        dims = cat_mr_dims_subtotals
    )
    expect_equivalent(prop.table(cat_mr, 1), row_prop)

    col_prop <- cubify(
        0.174694599107339, 0.210785476456778, 0.12614879593551, 0.168617769920104, 0.294784936205628,
        0.699159400497994, 0.703299886117777, 0.576266881620586, 0.646358853161181, 0.785568455152935,
        0.405619533756911, 0.292333374661432, 0.27779400156812, 0.297405340983255, 0.294270662571973,
        0.118845267633744, 0.200181034999566, 0.172324084116957, 0.180335742257822, 0.196512856375335,
        0.300840599502006, 0.296700113882224, 0.423733118379414, 0.353641146838819, 0.214431544847065,
        0.300840599502006, 0.296700113882224, 0.423733118379414, 0.353641146838819, 0.214431544847065,
        dims = cat_mr_dims_subtotals
    )
    expect_equivalent(prop.table(cat_mr, 2), col_prop)

    table_prop <- cubify(
        0.0264681177981324, 0.0835958238900571, 0.064598202124666, 0.137083692991725, 0.249826036054328,
        0.105930197422315, 0.278923075773934, 0.295094409903907, 0.525479957605977, 0.66575808020014,
        0.0614557385034259, 0.115937064147748, 0.142252590912489, 0.241786037597137, 0.249390196472437,
        0.0180063411207562, 0.0793901877361293, 0.0882436168667512, 0.146610227017115, 0.166541847673375,
        0.0455805987521531, 0.117668877786008, 0.216985009052181, 0.287504895987324, 0.181727681012776,
        0.0455805987521531, 0.117668877786008, 0.216985009052181, 0.287504895987324, 0.181727681012776,
        dims = cat_mr_dims_subtotals
    )
    expect_equivalent(prop.table(cat_mr), table_prop)
})

cat_array_cube <- loadCube("./cubes/catarray-with-transforms.json")

test_that("categorical arrays with subtotals", {
    all <- cubify(
        c(
            1, 2, 3,
            2, 1, 3,
            2, 1, 3
        ),
        dims = list(
            "CA" =
                c("mr_1", "mr_2", "mr_3"),
            "CA" = c("A", "B", "A+B")
        )
    )

    # pretty printing
    expect_equivalent(applyTransforms(cat_array_cube), all)
    skip_on_local_env("Pretty formatting isn't exactly the same in many terminals")
    expect_prints(
        cat_array_cube,
        paste(
            "    CA ",
            "CA     A   B \033[30m\033[3mA+B\033[23m\033[39m",
            "mr_1   1   2 \033[30m\033[3m  3\033[23m\033[39m",
            "mr_2   2   1 \033[30m\033[3m  3\033[23m\033[39m",
            "mr_3   2   1 \033[30m\033[3m  3\033[23m\033[39m",
            sep = "\n"
        ),
        fixed = TRUE
    )
})

test_that("categorical arrays with subtotals (margins and proportions)", {
    margin_1 <- cubify(
        c(3, 3, 3),
        dims = list("CA" = c("mr_1", "mr_2", "mr_3"))
    )
    expect_equivalent(margin.table(cat_array_cube, 1), margin_1)

    margin_2 <- cubify(
        c(5, 4, 9),
        dims = list("CA" = c("A", "B", "A+B"))
    )
    expect_equivalent(margin.table(cat_array_cube, 2), margin_2)

    expect_equivalent(
        prop.table(cat_array_cube, 1),
        applyTransforms(cat_array_cube) / broadcast(t(margin_1), ncol = 3)
    )

    expect_equivalent(
        prop.table(cat_array_cube, 2),
        applyTransforms(cat_array_cube) / broadcast(margin_2, dims = c(3, 3))
    )
})

test_that("subtotals after cube subsetting", {
    first_two_rows <- cubify(
        c(
            9, 9, 5, 14,
            12, 12, 12, 24,
            21, 21, 17, 38,
            0, 0, 0, 0
        ),
        dims = list(
            "feelings" =
                c("extremely happy", "somewhat happy", "happy", "unhappy"),
            "animals" = c("cats", "felines", "dogs", "both")
        )
    )

    expect_equivalent(applyTransforms(pet_feeling_both[c(1, 2), ]), first_two_rows)

    one_col <- cubify(
        c(
            5,  5,  0,
            12, 12, 0,
            17, 17, 0,
            7,  7,  0,
            10, 10, 0,
            12, 12, 0,
            22, 22, 0
        ),
        dims = list(
            "feelings" =
                c(
                    "extremely happy", "somewhat happy", "happy", "neutral",
                    "somewhat unhappy", "extremely unhappy", "unhappy"
                ),
            "animals" = c("dogs", "both", "felines")
        )
    )

    # need drop = FALSE to maintain the columns dimension since selection 1
    # usually removes the dimension
    subset <- pet_feeling_both[, c(2), drop = FALSE]
    expect_equivalent(
        applyTransforms(pet_feeling_both[, c(2), drop = FALSE]),
        one_col
    )

    one_col_withdrop <- cubify(
        c(
            5,
            12,
            17,
            7,
            10,
            12,
            22
        ),
        dims = list(
            "feelings" =
                c(
                    "extremely happy", "somewhat happy", "happy", "neutral",
                    "somewhat unhappy", "extremely unhappy", "unhappy"
                )
        )
    )

    expect_equivalent(applyTransforms(pet_feeling_both[, 2]), one_col_withdrop)
})


test_that("Can get subtotals alone", {
    subtotes <- cubify(
        c(
            21, 17,
            21, 22
        ),
        dims = list(
            "feelings" = c("happy", "unhappy"),
            "animals" = c("cats", "dogs")
        )
    )
    expect_equivalent(subtotalArray(pet_feelings_headers), subtotes)
})

test_that("Can get subtotals with headers", {
    subtotes <- cubify(
        c(
            NA, NA,
            21, 17,
            21, 22
        ),
        dims = list(
            "feelings" = c("Subtitle", "happy", "unhappy"),
            "animals" = c("cats", "dogs")
        )
    )

    expect_equivalent(subtotalArray(pet_feelings_headers, headings = TRUE), subtotes)
})

cat <- loadCube("cubes/cat-subtotals-0id.json")
cat_dims <- dimnames(cat)
# drop no data categories, and add in the subtotals
cat_dims$food_groups <- cat_dims$food_groups[!(cat_dims$food_groups %in% c("Don't know", "No Data", "Not asked"))]
cat_dims_subtotals <- cat_dims
cat_dims_subtotals$food_groups <- c(
    cat_dims_subtotals$food_groups[1], "plant-based",
    cat_dims_subtotals$food_groups[c(2, 3, 4)], "animal-based",
    "plant-based again, after animal-based"
)

test_that("subtotals with bases on weighted cube", {
    unweighted_counts <- cubify(
        434,
        1223,
        495,
        294,
        433,
        433,
        1223,
        dims = cat_dims_subtotals
    )
    expect_equivalent(bases(cat, 0), unweighted_counts)
    expect_equivalent(bases(cat), 1656)
})

##############################################################
### Transform interaction (getting, setting, anchors, etc)
##############################################################

test_that("can retrieve transformations from a cube", {
    trans <- list(
        "v7" = Transforms(
            insertions = Insertions(
                Subtotal(name = c("C, E"), after = 3, categories = c(1, 3)),
                Subtotal(name = c("D, E"), after = 3, categories = c(2, 3))
            ),
            categories = NULL,
            elements = NULL
        )
    )
    expect_equivalent(transforms(unicat_trans_cube), trans)
})

test_that("can remove transformations from a cube", {
    # without changing the cube
    expect_equal(
        transforms(noTransforms(unicat_trans_cube)),
        TransformsList("v7" = NULL))

    # with changing the cube
    transforms(unicat_trans_cube) <- NULL
    expect_equal(transforms(unicat_trans_cube), TransformsList("v7" = NULL))
})


transforms(pet_feelings) <- NULL
feelings_trans <- Transforms(
    insertions = Insertions(
        Heading(name = "Fabulous new header", position = "top"),
        Subtotal(name = "moderately happy",
                 after = "somewhat unhappy",
                 categories = c("somewhat happy", "neutral",
                                "somewhat unhappy"))
    ))
animals_trans <- Transforms(
    insertions = Insertions(
        Subtotal("felines", categories = "cats", after = "cats"),
        Subtotal("both", categories = c("cats", "dogs"), after = "dogs")
    )
)

test_that("can set transforms on a cube", {
    expect_equal(
        transforms(pet_feelings),
        TransformsList(feelings = NULL, animals = NULL))

    transforms(pet_feelings)[["feelings"]] <- feelings_trans

    # add empty elements/categories
    feelings_trans["elements"] <- feelings_trans["categories"] <- list(NULL)

    # convert to category ids
    feelings_trans$insertions[["moderately happy"]]$categories <- c(4L, 3L, 5L)
    feelings_trans$insertions[["moderately happy"]]$after <- 5L

    # ensure the transforms were set appropriately
    expect_equal(
        transforms(pet_feelings),
        TransformsList(feelings = feelings_trans, animals = NULL)
    )

    all <- cubify(
        c(
            NA, NA,
            9, 5,
            12, 12,
            12, 7,
            10, 10,
            34, 29,
            11, 12
        ),
        dims = list(
            "feelings" =
                c(
                    "Fabulous new header", "extremely happy",
                    "somewhat happy", "neutral",
                    "somewhat unhappy", "moderately happy",
                    "extremely unhappy"
                ),
            "animals" = c("cats", "dogs")
        )
    )
    expect_equivalent(applyTransforms(pet_feelings), all)

    expect_error(
        transforms(pet_feelings) <- list("not in the var" = Transforms(
            insertions = Insertions(
                Heading(name = "Fabulous new header", position = "top"),
                Subtotal(name = "subtotal", after = 2, categories = c(1, 2))
            )
        )),
        paste0(
            "The names of the transforms supplied .*not in the var.* do not",
            " match the dimensions of the cube .*feelings.* and .*animals.*"
        )
    )
})

test_that("can remove individual dimensions transforms", {
    transforms(pet_feelings)[["feelings"]] <- feelings_trans
    transforms(pet_feelings)[["animals"]] <- animals_trans

    # add empty elements/categories
    feelings_trans["elements"] <- feelings_trans["categories"] <- list(NULL)
    animals_trans["elements"] <- animals_trans["categories"] <- list(NULL)

    # convert to category ids
    feelings_trans$insertions[["moderately happy"]]$categories <- c(4L, 3L, 5L)
    feelings_trans$insertions[["moderately happy"]]$after <- 5L

    animals_trans$insertions[["felines"]]$categories <- 1L
    animals_trans$insertions[["felines"]]$after <- 1L
    animals_trans$insertions[["both"]]$categories <- c(1L, 2L)
    animals_trans$insertions[["both"]]$after <- 2L

    # ensure the transforms were set appropriately
    expect_equal(
        transforms(pet_feelings),
        TransformsList(feelings = feelings_trans, animals = animals_trans)
    )

    transforms(pet_feelings)[["feelings"]] <- NULL
    # ensure the transforms were removed from feelings
    expect_equal(
        transforms(pet_feelings),
        TransformsList(feelings = NULL, animals = animals_trans)
    )
})

test_that("can set transforms on a cube indexed by numerics", {
    expect_equal(
        transforms(pet_feelings),
        TransformsList(feelings = NULL, animals = NULL))
    transforms(pet_feelings)[[1]] <- feelings_trans

    # add empty elements/categories
    feelings_trans["elements"] <- feelings_trans["categories"] <- list(NULL)

    # convert to category ids
    feelings_trans$insertions[["moderately happy"]]$categories <- c(4L, 3L, 5L)
    feelings_trans$insertions[["moderately happy"]]$after <- 5L

    # ensure the transforms were set appropriately
    expect_equal(
        transforms(pet_feelings),
        TransformsList(feelings = feelings_trans, animals = NULL)
    )
})

test_that("subtotals with 0 anchor attach to 0 and not top", {
    all <- cubify(
        376.775218800139,
        1180.53898816961,
        485.589210635439,
        318.174558734034,
        471.521308228948,
        471.521308228948,
        1180.53898816961,
        dims = cat_dims_subtotals
    )
    expect_equivalent(applyTransforms(cat), all)
})


##############################################################
### Integration tests
##############################################################

with_test_authentication({
    df <- data.frame(pets = c(
        rep("Dogs", 50),
        rep("Cats", 45),
        rep("Birds", 30),
        rep("Lizards", 25),
        rep("Rocks", 5),
        rep(NA, 10)
    ))

    ds <- newDataset(df)

    # set rocks to be missing
    is.na(categories(ds$pets)) <- c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)

    insrts_list <- list(
        list(
            anchor = "top", name = "First one",
            `function` = "subtotal", args = c(1, 2)
        ),
        list(
            anchor = "bottom", name = "Toward the end",
            `function` = "subtotal", args = c(3, 4)
        ),
        list(
            anchor = 3, name = "Dogs+Cats",
            `function` = "subtotal", args = c(2, 3)
        ),
        list(
            anchor = 4, name = "Birds+Lizards",
            `function` = "subtotal", args = c(1, 4)
        ),
        list(
            anchor = 5, name = "Cats+Birds (missing anch.)",
            `function` = "subtotal", args = c(2, 1)
        ),
        list(
            anchor = "bottom", name = "Rocks+Birds (incl. missing)",
            `function` = "subtotal", args = c(5, 1)
        )
    )

    ## test variable-based methods
    test_that("showTransforms before a transform returns a the standard table", {
        cat_summary <- cubify(c(30, 45, 50, 25),
            dims = list(pets = c(
                "Birds", "Catds",
                "Dogs", "Lizards"
            ))
        )
        expect_prints(expect_equivalent(showTransforms(ds$pets), cat_summary))
    })

    # add transforms
    transforms(ds$pets) <- Transforms(insertions = Insertions(data = insrts_list))

    test_that("summary still works after adding transforms", {
        cat_summary <- cubify(c(50, 45, 30, 25),
            dims = list(
                pets = c(
                    "Dogs", "Catds",
                    "Birds", "Lizards"
                ),
                "Count"
            )
        )
        class(cat_summary) <- "CategoricalVariableSummary"

        expect_equivalent(summary(ds$pets), cat_summary)
    })

    test_that("showTransforms works on a variable", {
        skip("TODO: unskip")
        cat_show_trans <- cubify(c(75, 30, 45, 50, 95, 25, 55, 75, 75, NA),
            dims = list(pets = c(
                "First one", "Birds", "Cats", "Dogs",
                "Dogs+Cats", "Lizards", "Birds+Lizards",
                "Toward the end", "Cats+Birds (missing anch.)",
                "Rocks+Birds (incl. missing)"
            ))
        )
        trans_pets <- showTransforms(ds$pets)

        expect_is(trans_pets, "array")
        expect_equal(dim(trans_pets), 10)
        expect_equivalent(trans_pets, cat_show_trans)

        skip_on_local_env("Pretty formatting isn't exactly the same in many terminals")
        expect_prints(
            trans_pets <- showTransforms(ds$pets),
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
                sep = "\n"
            ),
            fixed = TRUE
        )
    })

    test_that("showTransforms works on a variable", {
        cat_show_trans <- cubify(c(75, 30, 45, 50, 95, 25, 55, 75, 75, 30),
            dims = list(pets = c(
                "First one", "Birds", "Cats", "Dogs",
                "Dogs+Cats", "Lizards", "Birds+Lizards",
                "Toward the end", "Cats+Birds (missing anch.)",
                "Rocks+Birds (incl. missing)"
            ))
        )

        pets_cube <- crtabs(~pets, ds)
        trans_cube <- applyTransforms(pets_cube)

        expect_is(trans_cube, "array")
        expect_equal(dim(showMissing(pets_cube)), 6)
        expect_equal(dim(pets_cube), 4)
        expect_equal(dim(trans_cube), 10)
        expect_equivalent(trans_cube, cat_show_trans)

        skip_on_local_env("Pretty formatting isn't exactly the same in many terminals")
        expect_prints(
            trans_cube <- showTransforms(pets_cube),
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
                "\033[30m\033[3mRocks+Birds (incl. missing) 35\033[23m\033[39m",
                sep = "\n"
            ),
            fixed = TRUE
        )
    })
})
