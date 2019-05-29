context("Handling append conflicts")

with_test_authentication({
    whereas("When attempting to append an array and a numeric", {
        part1 <- mrdf.setup(newDataset(mrdf))
        part2 <- newDataset(mrdf[c("mr_3", "v4")])
        alias(part2$mr_3) <- "CA"
        name(part2$CA) <- "Bad var"
        test_that("setup for append array type mismatch", {
            expect_length(batches(part1), 1)
            expect_true("CA" %in% names(part1))
            expect_true("CA" %in% names(part2))
            expect_true(is.CA(part1$CA))
            expect_true(is.Numeric(part2$CA))
            expect_prints(
                batches(part1),
                get_output(data.frame(
                    status = c("imported")
                ))
            )
        })
        test_that("compareDatasets catches that", {
            comp <- compareDatasets(part1, part2)
            expect_prints(summary(comp), "Type mismatch: 1")
        })
        test_that("If the append fails and reports conflict, and the batch is backed out", {
            expect_prints(
                batches(part1),
                get_output(data.frame(
                    status = c("imported")
                ))
            )

            expect_message(
                expect_error(
                    part1 <- appendDataset(part1, part2),
                    "Variable 'CA' is an array in one dataset and not the other"
                ),
                NA
            ) ## No Result URL printed because autorollback=TRUE

            expect_setequal(as.data.frame(batches(part1))$status, c("imported", "error"))
        })
    })

    whereas("When attempting to append text and numeric", {
        part1 <- newDataset(df[, 2:5])
        d2 <- df
        d2$v2 <- d2$v3 ## v2 was text, now is numeric
        part2 <- newDataset(d2)
        test_that("compareDatasets catches that", {
            comp <- compareDatasets(part1, part2)
            expect_prints(summary(comp), "Type mismatch: 1")
        })
        test_that("Append detects text/numeric type mismatch", {
            expect_error(
                appendDataset(part1, part2),
                "Type of 'v2' does not match target and cannot be converted."
            )
        })
    })
})
