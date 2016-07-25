context("Handling append conflicts")

c1 <- list()
c2 <- list(
    var1=list(
        conflicts=list(list(
            message="No good",
            resolution="But I fixed it already"
        )),
        metadata=list(
            references=list(
                name="First"
            )
        )
    ),
    var4=list(conflicts=list())
)
c3 <- list(
    var2=list(
        conflicts=list(list(
            message="No good",
            resolution="But I fixed it already"
        )),
        metadata=list(
            references=list(
                name="Second"
            )
        )
    ),
    var4=list(conflicts=list()),
    var1=list(
        conflicts=list(
            list(
                message="No good",
                resolution="But I fixed it already"
            ),
            list(
                message="Oh, and there was another problem",
                resolution="But it's also cool"
            )
        ),
        metadata=list(
            references=list(
                name="First"
            )
        )
    )
)
c4 <- c3
c4$var3 <- list(
    conflicts=list(list(
        message="Type mismatch"
    )),
    metadata=list(references=list(name="Last"))
)

test_that("Simple conflict messages are formatted correctly", {
    expect_equivalent(flattenConflicts(c3),
        data.frame(
            message=c("No good", "No good", "Oh, and there was another problem"),
            resolution=c("But I fixed it already", "But I fixed it already", "But it's also cool"),
            url=c("var2", "var1", "var1"),
            name=c("Second", "First", "First"),
            stringsAsFactors=FALSE))
        expect_equivalent(flattenConflicts(c4),
            data.frame(
                message=c("No good", "No good", "Oh, and there was another problem", "Type mismatch"),
                resolution=c("But I fixed it already", "But I fixed it already", "But it's also cool", NA),
                url=c("var2", "var1", "var1", "var3"),
                name=c("Second", "First", "First", "Last"),
                stringsAsFactors=FALSE))

    expect_identical(formatConflicts(flattenConflicts(c1)), "No conflicts.")
    expect_identical(formatConflicts(flattenConflicts(c2)),
        paste("Conflict: No good; Resolution: But I fixed it already; 1 variable:", dQuote("First")))
    expect_identical(formatConflicts(flattenConflicts(c3)),
        c(paste("Conflict: No good; Resolution: But I fixed it already; 2 variables:", dQuote("Second"), "and", dQuote("First")),
        paste("Conflict: Oh, and there was another problem; Resolution: But it's also cool; 1 variable:", dQuote("First"))))
    expect_identical(formatFailures(flattenConflicts(c4)),
        paste("Critical conflict: Type mismatch; 1 variable:", dQuote("Last")))
})

source("conflicts.R")
test_that("Complex conflicts are formatted", {
    expect_identical(formatConflicts(flattenConflicts(mock.conflicts)),
        c(paste("Conflict: Only in existing dataset; Resolution: Additional rows will be marked missing.; 1 variable:", dQuote("mr_1")),
        paste("Conflict: Only in new dataset; Resolution: Variable will be added with existing rows marked missing.; 1 variable:", dQuote("mr_3")),
        paste("Conflict: Subvariables didn't match; Resolution: Union of subvariables will be used; 1 variable:", dQuote("MR"))))
})

with_test_authentication({
    describe("When attempting to append an array and a numeric", {
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
        })
        test_that("The append fails and reports conflict on type mismatch", {
            expect_error(
                expect_message(appendDataset(part1, part2),
                    "Result URL"),
                "Variable is array in one dataset and not the other")
            skip("2 != 3")
            expect_length(batches(refresh(part1)), 2) ## The prospective batch was deleted
        })
    })

    test_that("Append detects text/numeric type mismatch", {
        part1 <- newDataset(df[,2:5])
        d2 <- df
        d2$v2 <- d2$v3 ## v2 was text, now is numeric
        part2 <- newDataset(d2)
        expect_error(appendDataset(part1, part2),
            "type text on current and of type numeric in incoming")
    })
})
