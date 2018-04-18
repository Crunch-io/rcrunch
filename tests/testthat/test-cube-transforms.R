context("Cube transformations")

unicat_trans_cube <- loadCube("cubes/univariate-categorical-with-trans.json")

test_that("Can show a simple cube with transform", {
    loc_array <- array(c(10, 5, 15, NA),
                     dimnames = list("v7" = c("C", "E", "C, E", "D, E")))
    expect_output(expect_equivalent(showTransforms(unicat_trans_cube), loc_array))
})

test_that("can retrieve transformations from a cube", {
    trans <- list(
        'v7' = Transforms(insertions = Insertions(
            Subtotal(name = c("C, E"), after = 6, categories = c(1, 3)),
            Subtotal(name = c("D, E"), after = 7, categories = c(2, 3))),
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

complex_trans_cube <- loadCube("cubes/complex-categorical-with-trans.json")

test_that("Can show a complex cube with transform", {
    loc_array <- array(c(40, 10, 20, 30, 30, 40, 50, 60, 70, 250,
                       80, 90, 100, 520, 150, NA),
                     dimnames = list("v7" = c("First!", "A", "B", "Top 2", "C",
                                              "D", "E", "F", "G", "Middle 5",
                                              "H", "I", "J", "Bottom 8",
                                              "Middle 3 (missing anchor)", "J and can't see")))
    expect_output(expect_equivalent(showTransforms(complex_trans_cube), loc_array))
})

pet_feelings <- pet_feelings_headers <- loadCube("./cubes/feelings-pets.json")

# add a header for some tests
new_trans <- pet_feelings_headers@dims$feelings$references$view$transform
new_trans$insertions <- c(new_trans$insertions, list(list(name = "Subtitle", anchor = 0)))
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
    cat_array_cube <- loadCube("./cubes/catarray-with-transforms.json")

    all <- array(c(1, 2, 2, 2, 1, 1),
                 dim = c(3, 2),
                 dimnames = list("CA" =
                                     c("mr_1", "mr_2", "mr_3"),
                                 "CA" = c("A", "B")))

    expect_equivalent(applyTransforms(cat_array_cube), all)
    expect_output(cat_array_cube,
                  "    CA\nCA    A  B\nmr_1  1  2\nmr_2  2  1\nmr_3  2  1")
})

test_that("can set transforms on a cube", {
    transforms(pet_feelings) <- NULL
    expect_null(transforms(pet_feelings))
    transforms(pet_feelings) <- list("feelings" = Transforms(
        insertions = Insertions(
            Heading(name = "Fabulous new header", position = "top"),
            Subtotal(name = "moderately happy",
                     after = "somewhat unhappy",
                     categories = c("somewhat happy", "neutral",
                                    "somewhat unhappy"))
        )))

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
        paste0("The names of the transforms supplied .*not in the var.* to not",
               " match the dimension names .*feelings.* and .*animals.* of the",
               " cube.")
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

    insrts_list <- list(list(anchor = 0, name = "First one",
                             `function` = "subtotal", args = c(1, 2)),
                        list(anchor = 999, name = "Toward the end",
                             `function` = "subtotal", args = c(3, 4)),
                        list(anchor = 3, name = "Dogs+Cats",
                             `function` = "subtotal", args = c(2, 3)),
                        list(anchor = 4, name = "Birds+Lizards",
                             `function` = "subtotal", args = c(1, 4)),
                        list(anchor = 5, name = "Cats+Birds (missing anch.)",
                             `function` = "subtotal", args = c(2, 1)),
                        list(anchor = 99, name = "Rocks+Birds (incl. missing)",
                             `function` = "subtotal", args = c(5, 1)))

    ## test variable-based methods
    test_that("showTransforms before a transform returns a the standard table", {
        cat_summary <- array(c(30, 45, 50, 25),
                             dim = 4,
                             dimnames = list(pets = c("Birds", "Catds",
                                                      "Dogs", "Lizards")))
        expect_output(expect_equivalent(showTransforms(ds$pets), cat_summary))
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
        expect_output(trans_pets <- showTransforms(ds$pets),
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
        expect_output(trans_cube <- showTransforms(pets_cube),
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
