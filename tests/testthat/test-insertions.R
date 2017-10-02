context("Insertions")

insrt <- Insertion(anchor = 6, name = "Low", `function` = list(combine = c(1, 2)))
insrts <- Insertions(data=list(list(anchor = 6, name = "Low",
                                    `function` = list(combine = c(1, 2))),
                               list(anchor = 7, name = "High",
                                    `function` = list(combine = c(9, 10)))))

test_that("Insertion and insertion inheritence, base methods", {
    expect_equal(anchor(insrt), 6)
    expect_equal(name(insrt), "Low")
    expect_equal(combinations(insrt), c(1, 2))
})

test_that("Insertion and insertions show methods", {
    expect_output(insrt,
                  get_output(data.frame(anchor=c(6),
                                        name=c("Low"),
                                        combine=c("1, 2"))))
    expect_output(insrts,
                  get_output(data.frame(anchor=c(6, 7),
                                        name=c("Low", "High"),
                                        combine=c("1, 2", "9, 10"))))
})

