context("Cube transformations")

unicat_trans_cube <- loadCube("cubes/univariate-categorical-with-trans.json")

test_that("Can show a simple cube with transform", {
    loc_ary <- array(c(10, 5, 15, NA),
                     dimnames = list("v7" = c("C", "E", "C, E", "D, E")))
    expect_equivalent(showTransforms(unicat_trans_cube), loc_ary)
})

test_that("can retrieve transformations from a cube", {
    trans <- Transforms(insertions = Insertions(data=list(
        list(anchor = 6, name = c("C, E"), `function` = "subtotal", args = c(1, 3)),
        list(anchor = 7, name = c("D, E"), `function` = "subtotal", args = c(2, 3))
    )))
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
    loc_ary <- array(c(40, 10, 20, 30, 30, 40, 50, 60, 70, 250,
                       80, 90, 100, 520, 150, NA),
                     dimnames = list("v7" = c("First!", "A", "B", "Top 2", "C",
                                              "D", "E", "F", "G", "Middle 5",
                                              "H", "I", "J", "Bottom 8",
                                              "Middle 3 (missing anchor)", "J and can't see")))
    expect_equivalent(showTransforms(complex_trans_cube), loc_ary)
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
        expect_equivalent(showTransforms(ds$pets), cat_summary)
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

        trans_pets <- showTransforms(ds$pets)
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
        trans_cube <- showTransforms(pets_cube)
        expect_is(trans_cube, "array")
        expect_equal(dim(pets_cube), 6)
        expect_equal(dim(trans_cube), 10)
        expect_equivalent(trans_cube, cat_show_trans)
    })
})
