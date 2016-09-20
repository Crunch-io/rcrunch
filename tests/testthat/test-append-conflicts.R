context("Handling append conflicts")

with_test_authentication({
    whereas("When attempting to append an array and a numeric", {
        part1 <- mrdf.setup(newDataset(mrdf))
        part2 <- newDataset(mrdf[c("mr_3", "v4")])
        alias(part2$mr_3) <- "CA"
        name(part2$CA) <- "Bad var"
        test_that("setup for append array type mismatch", {
            expect_length(batches(part1), 2)
            expect_true("CA" %in% names(part1))
            expect_true("CA" %in% names(part2))
            expect_true(is.CA(part1$CA))
            expect_true(is.Numeric(part2$CA))
            expect_output(batches(part1),
                get_output(data.frame(id=c(0, 1),
                status=c("imported", "imported"))))
        })
        test_that("compareDatasets catches that", {
            comp <- compareDatasets(part1, part2)
            expect_output(summary(comp), "Type mismatch: 1")
        })
        test_that("With autorollback, the append fails and reports conflict, and the batch is backed out", {
            expect_output(batches(part1),
                get_output(data.frame(id=c(0, 1),
                status=c("imported", "imported"))))
            expect_error(
                expect_message(part1 <- appendDataset(part1, part2),
                    "Result URL"),
                "Variable is array in one dataset and not the other")
            part1 <- refresh(part1)
            expect_output(batches(part1),
                get_output(data.frame(id=c(0, 1),
                status=c("imported", "imported"))))
        })
        test_that("The append fails and reports conflict on type mismatch", {
            expect_error(
                expect_message(appendDataset(part1, part2, autorollback=FALSE),
                    "Result URL"),
                "Variable is array in one dataset and not the other")
        })
        
        part1 <- refresh(part1)
        test_that("There is an 'error' batch, and cleanseBatches purges the errored batch", {
            expect_output(batches(part1),
                get_output(data.frame(id=c(0, 1, 2),
                status=c("imported", "imported", "error"))))
            part1 <- cleanseBatches(part1)
            expect_output(batches(part1),
                get_output(data.frame(id=c(0, 1),
                status=c("imported", "imported"))))
        })
    })

    whereas("When attempting to append text and numeric", {
        part1 <- newDataset(df[,2:5])
        d2 <- df
        d2$v2 <- d2$v3 ## v2 was text, now is numeric
        part2 <- newDataset(d2)
        test_that("compareDatasets catches that", {
            comp <- compareDatasets(part1, part2)
            expect_output(summary(comp), "Type mismatch: 1")
        })
        test_that("Append detects text/numeric type mismatch", {
            expect_error(
                expect_message(appendDataset(part1, part2),
                    "Result URL"),
                "Type of 000001 does not match target 000000 and cannot be converted.")
        })
    })
})
