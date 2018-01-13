context("Insertions")

insrt <- Insertion(anchor = 6, name = "Low", `function` = "subtotal", args = c(1, 2))
insrts <- Insertions(data=list(list(anchor = 6, name = "Low",
                                    `function` = "subtotal", args = c(1, 2)),
                               list(anchor = 7, name = "High",
                                    `function` = "subtotal", args = c(9, 10))))

test_that("Insertion and insertion inheritence, base methods", {
    expect_equal(anchor(insrt), 6)
    expect_equal(name(insrt), "Low")
    expect_equal(args(insrt), c(1, 2))

    expect_equal(anchors(insrts), c(6, 7))
    expect_equal(funcs(insrts), c('subtotal', 'subtotal'))
})

insrt2 <- insrt

test_that("Insertion setters", {
    anchor(insrt2) <- 1
    expect_equal(anchor(insrt2), 1)
    name(insrt2) <- "Low low"
    expect_equal(name(insrt2), "Low low")
    subtotals(insrt2) <- c(10, 20)
    expect_equal(args(insrt2), c(10, 20))
})

test_that("Insertion can take an anchor of int, top, or bottom", {
    anchor(insrt2) <- "top"
    expect_equal(anchor(insrt2), "top")

    anchor(insrt2) <- "bottom"
    expect_equal(anchor(insrt2), "bottom")

    anchor(insrt2) <- 4
    expect_equal(anchor(insrt2), 4)
})

test_that("Anchors can be converted from subtotal/header to insertion", {
    sub <- Subtotal(name = "name", categories = c(1, 2), after = 1)
    sub_top <- Subtotal(name = "name", categories = c(1, 2), position = "top")
    sub_bottom <- Subtotal(name = "name", categories = c(1, 2), position = "bottom")
    # TODO: check category names with a categories object

    expect_equal(anchor(sub), 1)
    expect_equal(anchor(sub_top), "top")
    expect_equal(anchor(sub_bottom), "bottom")
})

test_that("Insertion setter validation", {
    expect_error(anchor(insrt2) <- "one",
        paste0("an anchor must be a numeric or the character ", dQuote("top"),
            " or ", dQuote("bottom")))
    expect_error(name(insrt2) <- 2, 'Names must be of class "character"')
    expect_error(subtotals(insrt2) <- "3, 4", "a subtotal must be a numeric")
})

test_that("Insertion validation", {
    expect_error(Insertion(anchor=0),
                 "invalid class .*Insertion.* object:.* Missing: .*name*")
    expect_error(Insertion(name='bar'),
                 "invalid class .*Insertion.* object:.* Missing: .*anchor*")
    expect_error(Insertion(anchor=0, name='bar', `function`='baz'),
                 "If an Insertion has a .*function.* it must also have .*args.*")
})

test_that("Insertion and insertions show methods", {
    expect_output(insrt,
                  get_output(data.frame(anchor=c(6),
                                        name=c("Low"),
                                        func=c("subtotal"),
                                        args=c("1 and 2"))))
    expect_output(insrts,
                  get_output(data.frame(anchor=c(6, 7),
                                        name=c("Low", "High"),
                                        func=c("subtotal", "subtotal"),
                                        args=c("1 and 2", "9 and 10"))))
})

test_that("Insertion and insertions show methods with hetrogeneous insertions", {
    insrts <- Insertions(Subtotal(name = "Cats A+B", after = "B",
                                  categories = c("A", "B")),
                         Heading(name = "The end", after = "D"))

    expect_output(insrts,
                  get_output(data.frame(
                     anchor = c("B", "D"),
                     name = c("Cats A+B", "The end"),
                     func = c("subtotal", NA),
                     # NA is a string because we serialPaste them
                     args = c("A and B", "NA"))),
                  fixed = TRUE)
})

test_that("args returns NA when not found", {
    expect_equal(args(Insertion(anchor='foo', name='bar')), NA)
})
