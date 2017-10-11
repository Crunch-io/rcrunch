context("Insertions")

insrt <- Insertion(anchor = 6, name = "Low", `function` = "subtotal", args = c(1, 2))
insrts <- Insertions(data=list(list(anchor = 6, name = "Low",
                                    `function` = "subtotal", args = c(1, 2)),
                               list(anchor = 7, name = "High",
                                    `function` = "subtotal", args = c(9, 10))))

test_that("Insertion and insertion inheritence, base methods", {
    expect_equal(anchor(insrt), 6)
    expect_equal(name(insrt), "Low")
    expect_equal(subtotals(insrt), c(1, 2))
})

insrt2 <- insrt

test_that("Insertion setters", {
    anchor(insrt2) <- 1
    expect_equal(anchor(insrt2), 1)
    name(insrt2) <- "Low low"
    expect_equal(name(insrt2), "Low low")
    subtotals(insrt2) <- c(10, 20)
    expect_equal(subtotals(insrt2), c(10, 20))
})

test_that("Insertion setter validation", {
    expect_error(anchor(insrt2) <- "one", "an anchor must be a numeric")
    expect_error(name(insrt2) <- 2, 'Names must be of class "character"')
    expect_error(subtotals(insrt2) <- "3, 4", "a subtotal must be a numeric")
})

test_that("Insertion validation", {
    expect_error(Insertion(anchor='foo', `function`=list('baz')),
                 "invalid class .*Insertion.* object:.* Missing: .*name*")
    expect_error(Insertion(name='bar', `function`=list('baz')),
                 "invalid class .*Insertion.* object:.* Missing: .*anchor*")
})

test_that("Insertion and insertions show methods", {
    expect_output(insrt,
                  get_output(data.frame(anchor=c(6),
                                        name=c("Low"),
                                        subtotal=c("1, 2"))))
    expect_output(insrts,
                  get_output(data.frame(anchor=c(6, 7),
                                        name=c("Low", "High"),
                                        subtotal=c("1, 2", "9, 10"))))
})

test_that("subtotals returns NA when not found", {
    expect_equal(subtotals(Insertion(anchor='foo', name='bar',
                                        `function`=list('baz'))), NA)
})
