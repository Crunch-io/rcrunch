context("Summary statistic insertions")

test_that("SummaryStats accepts a variety of inputs", {
    mean1 <- SummaryStat(
        name = "Approval", stat = "mean",
        categories = c(1, 2), after = 2
    )
    expect_true(is.SummaryStat(mean1))

    mean2 <- SummaryStat(name = "Approval2", stat = "mean", after = 2)
    expect_true(is.SummaryStat(mean2))

    mean3 <- SummaryStat(
        name = "mean", stat = "mean",
        categories = c(1, 2)
    )
    expect_true(is.SummaryStat(mean3))
    expect_message(
        expect_equal(anchor(mean3), NA_integer_),
        paste0(
            "Can't determine the anchor position without a ",
            "variable. However, when this is added to a Crunch ",
            "variable or CrunchCube it will follow the last ",
            "category given"
        )
    )

    median1 <- SummaryStat(
        name = "Approval3", stat = "median",
        categories = c(2, 3), after = 2
    )

    expect_true(is.SummaryStat(median1))

    expect_true(all(are.SummaryStats(Insertions(mean1, mean2, median1))))
})

test_that("SummaryStat validates", {
    expect_error(
        SummaryStat(categories = c(1, 2), stat = "mean", after = 2),
        "argument \"name\" is missing, with no default"
    )
    expect_error(
        SummaryStat(name = "mean", categories = c(1, 2), after = 2),
        "argument \"stat\" is missing, with no default"
    )

    expect_error(
        SummaryStat(
            name = "not a stat", stat = "not a stat",
            after = 2
        ),
        paste0(
            ".*not a stat.* is not a known summary statistic for ",
            "insertions. Available stats are: mean and median"
        )
    )
})


all_mean <- SummaryStat(name = "mean", stat = "mean", after = 1)
one_two_mean <- SummaryStat(
    name = "mean", stat = "mean", after = 1,
    categories = c(1, 2)
)
all_median <- SummaryStat(name = "median", stat = "median", after = 1)

test_that("SummaryStat attribute getters", {
    expect_equal(func(all_mean), "mean")
    expect_equal(func(all_median), "median")
    expect_equal(arguments(one_two_mean), c(1, 2))
})

test_that("SummaryStat setters", {
    # names
    name(all_mean) <- "new name"
    expect_equal(name(all_mean), "new name")

    # arguments
    arguments(all_mean) <- c(2, 3)
    expect_equal(arguments(all_mean), c(2, 3))
    arguments(one_two_mean) <- c(2, 3)
    expect_equal(arguments(one_two_mean), c(2, 3))

    # anchors
    anchor(all_mean) <- 2
    expect_equal(anchor(all_mean), 2)
    expect_equal(all_mean$position, "relative")
    anchor(all_mean) <- "bottom"
    # anchor grabs position when after is null
    expect_equal(anchor(all_mean), "bottom")
    expect_equal(all_mean$position, "bottom")
    # after is null when position is anything other than relative
    expect_null(all_mean$after)
})

pet_feelings <- loadCube("./cubes/feelings-pets.json")
pet_feelings_w <- loadCube("./cubes/feelings-pets-weighted.json")

test_that("can set and calc a mean insertion", {
    # remove subtotals
    transforms(pet_feelings) <- NULL
    transforms(pet_feelings_w) <- NULL

    # there are no transforms (yet!)
    expect_equal(
        transforms(pet_feelings),
        TransformsList(feelings = NULL, animals = NULL))
    expect_equal(
        transforms(pet_feelings_w),
        TransformsList(feelings = NULL, animals = NULL))

    # add transforms
    pet_feelings <- addSummaryStat(pet_feelings, stat = "mean", var = "feelings")
    pet_feelings_w <- addSummaryStat(pet_feelings_w, stat = "mean", var = "feelings")


    feelings_trans <- Transforms(
        insertions = Insertions(
            SummaryStat(
                name = "mean", stat = "mean", position = "bottom",
                categories = c(1L, 4L, 3L, 5L, 2L, -1L)
            )
        ),
        elements = NULL,
        categories = NULL
    )

    expect_json_equivalent(
        transforms(pet_feelings),
        list("feelings" = feelings_trans, "animals" = NULL)
    )

    # check that they are calculated and added in the correct place.
    base_cube <- as.array(pet_feelings)
    calced_means <- apply(base_cube, 2, weighted.mean, x = c(10, 7.5, 5, 2.5, 0))
    expect_equivalent(
        applyTransforms(pet_feelings),
        rbind(base_cube, "mean" = calced_means)
    )

    # with a weighted cube
    expect_json_equivalent(
        transforms(pet_feelings_w),
        list("feelings" = feelings_trans, "animals" = NULL)
    )

    # check that they are calculated and added in the correct place.
    base_cube <- as.array(pet_feelings_w)
    calced_means <- apply(base_cube, 2, weighted.mean, x = c(10, 7.5, 5, 2.5, 0))
    expect_equivalent(
        applyTransforms(pet_feelings_w),
        rbind(base_cube, "mean" = calced_means)
    )
})

test_that("can set and calc a median insertion", {
    # remove subtotals
    transforms(pet_feelings) <- NULL
    transforms(pet_feelings_w) <- NULL

    # there are no transforms (yet!)
    expect_equal(
        transforms(pet_feelings),
        TransformsList(feelings = NULL, animals = NULL))

    # add transforms
    pet_feelings <- addSummaryStat(pet_feelings, stat = "median", var = "feelings")
    pet_feelings_w <- addSummaryStat(pet_feelings_w, stat = "median", var = "feelings")

    # make expectations object
    feelings_trans <- Transforms(
        insertions = Insertions(
            SummaryStat(
                name = "median", stat = "median",
                categories = c(1L, 4L, 3L, 5L, 2L, -1L),
                position = "bottom"
            )
        ),
        elements = NULL,
        categories = NULL
    )


    expect_json_equivalent(
        transforms(pet_feelings),
        list("feelings" = feelings_trans, "animals" = NULL)
    )

    # check that they are calculated and added in the correct place.
    base_cube <- as.array(pet_feelings)
    expect_equivalent(
        applyTransforms(pet_feelings),
        rbind(base_cube, "median" = c(5, 5))
    )

    # with a weighted cube too
    expect_json_equivalent(
        transforms(pet_feelings_w),
        list("feelings" = feelings_trans, "animals" = NULL)
    )

    # check that they are calculated and added in the correct place.
    base_cube <- as.array(pet_feelings_w)
    expect_equivalent(
        applyTransforms(pet_feelings_w),
        rbind(base_cube, "median" = c(10, 0))
    )
})

test_that("can set and calc a mean insertion that ignores missings", {
    # hack the cube as if extremely unhappy was missing
    pet_feelings@dims$feelings$missing <- c(rep(FALSE, 4), rep(TRUE, 2))
    pet_feelings@dims$feelings$references$categories[[4]]$missing <- TRUE

    # remove subtotals
    transforms(pet_feelings) <- NULL

    # add transforms
    pet_feelings <- addSummaryStat(pet_feelings, stat = "mean", var = "feelings")

    # check that they are calculated and added in the correct place.
    base_cube <- as.array(pet_feelings)
    calced_means <- apply(base_cube, 2, weighted.mean, x = c(10, 7.5, 5, 2.5))
    expect_equivalent(
        applyTransforms(pet_feelings),
        rbind(base_cube, "mean" = calced_means)
    )
})

cat_array <- loadCube("./cubes/cat-array.json")

test_that("can set and calc a mean insertion with catarrays", {
    # there are no transforms (yet!)
    expect_equal(
        transforms(cat_array),
        TransformsList(feeling_ca = NULL, feeling_ca = NULL))

    # add transforms
    cat_array <- addSummaryStat(cat_array, stat = "mean", margin = 2)

    feelings_trans <- Transforms(insertions = Insertions(
        SummaryStat(name = "mean", stat = "mean", position = "bottom",
                    categories = c(1L, 4L, 3L, 5L, 2L, -1L))),
        elements = NULL,
        categories = NULL)

    expect_json_equivalent(transforms(cat_array),
                           TransformsList(feeling_ca = NULL, feeling_ca = feelings_trans))

    # check that they are calculated and added in the correct place.
    base_cube <- as.array(cat_array)
    calced_means <- apply(base_cube, 1, weighted.mean, x = c(1, 2, 3, 4, 5))
    expect_equivalent(applyTransforms(cat_array),
                      cbind(base_cube, "mean" = calced_means))
})

test_that("can set and calc a mean for two dimensions", {
    # remove subtotals
    transforms(pet_feelings) <- NULL

    # there are no transforms (yet!)
    expect_equal(
        transforms(pet_feelings),
        TransformsList(feelings = NULL, animals = NULL))

    # add transforms
    pet_feelings <- addSummaryStat(pet_feelings, stat = "mean", margin = c(1, 2))

    feelings_trans <- Transforms(insertions = Insertions(
        SummaryStat(name = "mean", stat = "mean", position = "bottom",
                    categories = c(1L, 4L, 3L, 5L, 2L, -1L))),
        elements = NULL,
        categories = NULL)

    animal_trans <- Transforms(insertions = Insertions(
        SummaryStat(name = "mean", stat = "mean", position = "bottom",
                    categories = c(1L, 2L, -1L))),
        elements = NULL,
        categories = NULL)

    expect_json_equivalent(
        transforms(pet_feelings),
        list("feelings" = feelings_trans,"animals" = animal_trans)
    )

    # check that they are calculated and added in the correct place.
    base_cube <- as.array(pet_feelings)
    row_means <- c(apply(base_cube, 1, weighted.mean, x = c(1, 2)), NA)
    col_means <- apply(base_cube, 2, weighted.mean, x = c(10, 7.5, 5, 2.5, 0))
    expect_equivalent(applyTransforms(pet_feelings),
                      cbind(rbind(base_cube, "mean" = col_means), "mean" = row_means))

    # with a weighted cube
    transforms(pet_feelings_w) <- NULL

    # there are no transforms (yet!)
    expect_equal(
        transforms(pet_feelings_w),
        TransformsList(feelings = NULL, animals = NULL))

    # add transforms
    pet_feelings_w <- addSummaryStat(pet_feelings_w, stat = "mean", margin = c(1, 2))

    expect_json_equivalent(
        transforms(pet_feelings_w),
        list("feelings" = feelings_trans, "animals" = animal_trans)
    )

    # check that they are calculated and added in the correct place.
    base_cube <- as.array(pet_feelings_w)
    row_means <- c(apply(base_cube, 1, weighted.mean, x = c(1, 2)), NA)
    col_means <- apply(base_cube, 2, weighted.mean, x = c(10, 7.5, 5, 2.5, 0))
    expect_equivalent(applyTransforms(pet_feelings_w),
                      cbind(rbind(base_cube, "mean" = col_means), "mean" = row_means))
})

test_that("can set and calc a mean insertion, and maintain subtotals", {
    # there are transforms already
    expect_length(transforms(pet_feelings)[[1]]$insertions, 2)

    # add transforms
    pet_feelings <- addSummaryStat(pet_feelings,
        stat = "mean",
        var = "feelings"
    )
    expect_length(transforms(pet_feelings)[[1]]$insertions, 3)
    stat_insert <- transforms(pet_feelings)[[1]]$insertions[[3]]
    expect_equal(name(stat_insert), "mean")
    expect_equal(func(stat_insert), "mean")
    expect_equal(arguments(stat_insert), c(1L, 4L, 3L, 5L, 2L, -1L))
    expect_equal(anchor(stat_insert), "bottom")

    # generate the cube without the means
    cube_with_subtotals <- applyTransforms(pet_feelings,
        include = c(
            "subtotals", "headings",
            "cube_cells"
        )
    )

    # caculate means from the base array (necesarily without subtotals)
    base_cube <- as.array(pet_feelings)
    calced_means <- apply(base_cube, 2, weighted.mean, x = c(10, 7.5, 5, 2.5, 0))

    # check that the cube contains all of the cube (subtotals and cubecells) as
    # well as externally calculated means
    expect_equivalent(
        applyTransforms(pet_feelings),
        rbind(cube_with_subtotals, "mean" = calced_means)
    )
    # manual assertions equivalent to above.
    expect_equivalent(
        applyTransforms(pet_feelings),
        cubify(c(
            9, 5,
            12, 12,
            21, 17,
            12, 7,
            10, 10,
            11, 12,
            21, 22,
            4.90740740740741, 4.34782608695652
        ),
        dims = list(
            "feelings" = list(
                "extremely happy",
                "somewaht happy",
                "happy",
                "neutral",
                "somewhat unhappy",
                "extremely unhappy",
                "unhappy",
                "mean"
            ),
            "animals" = list("cats", "dogs")
        )
        )
    )
})

test_that("addSummaryStat validates", {
    expect_error(
        addSummaryStat(pet_feelings, stat = "not a stat", var = "feelings"),
        "'arg' should be one of .*mean.*, .*median.*"
    )
    expect_error(
        addSummaryStat(pet_feelings, var = "not a dim"),
        paste0(
            "The names of the variables supplied (.*not a dim.*) ",
            "do not match the dimensions of the cube (.*feelings.* and ",
            ".*animals.*)."
        )
    )
    expect_error(
        addSummaryStat(cat_array, stat = "mean", margin = 4),
        "Margin 4 exceeds Cube's number of dimensions (2)",
        fixed = TRUE
    )
})

test_that("meanInsert function calculates weighted means", {
    insertion <- SummaryStat(name = "mean", stat = "mean", position = "bottom")
    # remove nodata, which would be inside the cube calc functions
    var_cats <- categories(variables(pet_feelings)[["feelings"]])[1:5]
    vector_from_cube <- as.array(pet_feelings)[, 1]
    expect_equal(
        meanInsert(insertion, var_cats)(vector_from_cube),
        weighted.mean(c(10, 7.5, 5, 2.5, 0), c(9, 12, 12, 10, 11))
    )
})

test_that("SummaryStat defaults to following the last category given (like subtotals)", {
    insertion <- SummaryStat(name = "mean", stat = "mean", categories = c(1, 2))
    var_cats <- categories(variables(pet_feelings)[["feelings"]])
    expect_equal(anchor(insertion, var_cats), 2)
})

test_that("medianInsert function calculates weighted medians", {
    insertion <- SummaryStat(name = "median", stat = "median", position = "top")
    var_cats <- categories(variables(pet_feelings)[["feelings"]])[1:5]
    vector_from_cube <- as.array(pet_feelings)[, 1]
    expect_equal(
        medianInsert(insertion, var_cats)(vector_from_cube),
        5
    )

    # if there is an even number, and the median straddles two values, we get
    # the correct answer.
    var_cats <- categories(variables(pet_feelings)[["feelings"]])[c(2, 3)]
    vector_from_cube <- c("somewhat happy" = 12, "neutral" = 12)
    expect_equal(
        medianInsert(insertion, var_cats)(vector_from_cube),
        6.25
    )
})


test_that("mean and median value redaction works with a 3-dimensional cube", {
    three_d_cube <- loadCube("cubes/cat-x-cat-x-cat.json")

    # dim one
    three_d_cube_one <- addSummaryStat(three_d_cube, stat = "mean", margin = 1)

    # values are unchanged
    expect_equal(
        applyTransforms(three_d_cube_one)[c(1:6), , ],
        as.array(noTransforms(three_d_cube_one))
    )

    # the means are correct
    expect_equal(
        applyTransforms(three_d_cube_one)[7, , ],
        apply(
            as.array(noTransforms(three_d_cube_one)),
            MARGIN = c(2, 3),
            FUN = function (w) weighted.mean(c(1, 2, 0, 4, 5, 6), w = w)
            )
    )

    # dim two
    three_d_cube_two <- addSummaryStat(three_d_cube, stat = "mean", margin = 2)

    # values are unchanged
    expect_equal(
        applyTransforms(three_d_cube_two)[, c(1:4), ],
        as.array(noTransforms(three_d_cube_two))
    )

    # the means are correct
    expect_equal(
        applyTransforms(three_d_cube_two)[, 5, ],
        apply(
            as.array(noTransforms(three_d_cube_two)),
            MARGIN = c(1, 3),
            FUN = function (w) weighted.mean(c(0, 2, 5, 4), w = w)
        )
    )

    # dim three
    three_d_cube_three <- addSummaryStat(three_d_cube, stat = "mean", margin = 3)

    # values are unchanged
    expect_equal(
        applyTransforms(three_d_cube_three)[, , c(1:8)],
        as.array(noTransforms(three_d_cube_three))
    )

    # the means are correct
    expect_equal(
        applyTransforms(three_d_cube_three)[, , 9],
        apply(
            as.array(noTransforms(three_d_cube_three)),
            MARGIN = c(1, 2),
            FUN = function (w) weighted.mean(c(1, 2, 3, 4, 5, 6, 7, 8), w = w)
        )
    )

    # dim one, two, and three
    three_d_cube_123 <- addSummaryStat(three_d_cube, stat = "mean", margin = c(1, 2, 3))

    # values are unchanged
    expect_equal(
        applyTransforms(three_d_cube_123)[c(1:6), c(1:4), c(1:8)],
        as.array(noTransforms(three_d_cube_123))
    )

    # the means are correct
    expect_equal(
        applyTransforms(three_d_cube_123)[7, c(1:4), c(1:8)],
        apply(
            as.array(noTransforms(three_d_cube_123)),
            MARGIN = c(2, 3),
            FUN = function (w) weighted.mean(c(1, 2, 0, 4, 5, 6), w = w)
        )
    )

    expect_equal(
        applyTransforms(three_d_cube_123)[c(1:6), 5, c(1:8)],
        apply(
            as.array(noTransforms(three_d_cube_123)),
            MARGIN = c(1, 3),
            FUN = function (w) weighted.mean(c(0, 2, 5, 4), w = w)
        )
    )

    expect_equal(
        applyTransforms(three_d_cube_123)[c(1:6), c(1:4), 9],
        apply(
            as.array(noTransforms(three_d_cube_123)),
            MARGIN = c(1, 2),
            FUN = function (w) weighted.mean(c(1, 2, 3, 4, 5, 6, 7, 8), w = w)
        )
    )

})
