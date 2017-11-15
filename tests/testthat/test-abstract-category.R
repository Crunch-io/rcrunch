context("Abstract Categories")

insrts <- Insertions(data=list(list(anchor = 6, name = "Low",
                                    `function` = "subtotal", args = c(1, 2)),
                               list(anchor = 7, name = "High",
                                    `function` = "subtotal", args = c(9, 10))))

insrts2 <- Insertions(data=list(list(anchor = 10, name = "High"),
                                list(anchor = 1, name = "New one")))


test_that("modifyCats works like modifyList", {
    # modifyCats takes two AbsCats objects, and adds elements from the second to
    # the first, replacing any that have the same name in both.
    new_insrts <- modifyCats(insrts, insrts2)
    expect_is(new_insrts, "AbsCats")
    expect_is(new_insrts, "Insertions")
    expect_equivalent(new_insrts,
                      AbsCats(data = list(list(anchor = 6, name = "Low",
                                               `function` = "subtotal",
                                               args = c(1, 2)),
                                          list(anchor = 10, name = "High"),
                                          list(anchor = 1, name = "New one"))))
})

test_that("abstract category is.* functions", {
    expect_true(is.AbsCats(insrts))
    expect_true(is.AbsCat(insrts[[1]]))
})

test_that("abstract category [ and [[ getters", {
    expect_equal(insrts["Low"], insrts[1])
    expect_error(insrts["not here"], "subscript out of bounds: not here")
    expect_equal(insrts[["Low"]], insrts[[1]])
    expect_error(insrts[["not here"]], "subscript out of bounds: not here")
})

test_that("abstract category [ s etter", {
    new_insrt <- Insertion(data =list(anchor = 0, name = "here now"))
    insrts["here now"] <- Insertions(new_insrt)
    expect_equal(insrts[["here now"]], new_insrt)

    old_insrt <- Insertion(data = list(anchor = 60, name = "Low",
                                       `function` = "subtotal", args = c(10, 20)))
    insrts["Low"] <- Insertions(old_insrt)
    expect_equal(insrts[["Low"]], old_insrt)
})

test_that("abstract category [[ setter", {
    new_insrt <- Insertion(data=list(anchor = 0, name = "here now"))
    insrts[["here now"]] <- new_insrt
    expect_equal(insrts[["here now"]], new_insrt)

    old_insrt <- Insertion(data = list(anchor = 60, name = "Low",
                                       `function` = "subtotal", args = c(10, 20)))
    insrts[["Low"]] <- old_insrt
    expect_equal(insrts[["Low"]], old_insrt)
})

test_that("When used with NULL, func and anchor return NA", {
    expect_equal(func(NULL), NA_character_)
    expect_equal(anchor(NULL), NA_character_)
})
