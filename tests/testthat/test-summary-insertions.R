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

pet_feelings <- loadCube(test_path("./cubes/feelings-pets.json"))
pet_feelings_w <- loadCube(test_path("./cubes/feelings-pets-weighted.json"))

test_that("can set and calc a mean insertion", {
    # remove subtotals
    transforms(pet_feelings) <- NULL
    transforms(pet_feelings_w) <- NULL

    # there are no transforms (yet!)
    expect_null(transforms(pet_feelings))

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
    expect_null(transforms(pet_feelings))

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
        addSummaryStat(pet_feelings,
            stat = "not a stat",
            var = "feelings"
        ),
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
})

test_that("meanInsert function calculates weighted means", {
    insertion <- SummaryStat(name = "mean", stat = "mean", position = "bottom")
    var_cats <- categories(variables(pet_feelings)[["feelings"]])
    vector_from_cube <- as.array(pet_feelings)[, 1]
    expect_equal(
        meanInsert(insertion, var_cats, vector_from_cube),
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
    var_cats <- categories(variables(pet_feelings)[["feelings"]])
    vector_from_cube <- as.array(pet_feelings)[, 1]
    expect_equal(
        medianInsert(insertion, var_cats, vector_from_cube),
        5
    )

    # if there is an even number, and the median straddles two values, we get
    # the correct answer.
    var_cats <- categories(variables(pet_feelings)[["feelings"]])[c(2, 3)]
    vector_from_cube <- c("somewhat happy" = 12, "neutral" = 12)
    expect_equal(
        medianInsert(insertion, var_cats, vector_from_cube),
        6.25
    )
})
