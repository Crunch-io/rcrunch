context("Appends with sparse data")

with_test_authentication({
    part1 <- newDataset(data.frame(
        v4 = factor(rep(c("a", "b"), 500)),
        v5 = factor(rep(c("a", "b"), 500))
    ))
    part2 <- mrdf.setup(newDataset(data.frame(
        mr_1 = c(1, 0, 1, 1, 0, rep(NA, 995)),
        mr_2 = c(rep(NA, 995), 0, 1, 1, 1, 0),
        v4 = as.factor(LETTERS[2:3])
    )))
    test_that("compareDatasets identifies the category id mismatch", {
        expect_prints(
            summary(compareDatasets(part1, part2)),
            paste(
                "Mismatched ids: 4 ",
                " id.A name id.B",
                "    1    a   NA",
                "    2    b   NA",
                "   NA    B    1",
                "   NA    C    2",
                sep = "\n"
            )
        )
    })
    out <- suppressMessages(try(appendDataset(part1, part2)))
    test_that("the sparse arrays append", {
        expect_length(batches(out), 2)
        expect_identical(nrow(out), 2000L)
        expect_identical(
            as.vector(out$CA$mr_2),
            factor(c(rep(NA, 1995), "0.0", "1.0", "1.0", "1.0", "0.0"))
        )
    })

    test_that("the sparse categorical appends", {
        expect_identical(
            as.vector(out$v5),
            factor(c(rep(c("a", "b"), 500), rep(NA, 1000)))
        )
    })
})
