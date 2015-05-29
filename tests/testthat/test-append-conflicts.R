context("Displaying append conflicts")

c1 <- list()
c2 <- list(var1=list(
    conflicts=list(list(
        message="No good",
        resolution="But I fixed it already"
    )),
    metadata=list(
        name="First"
    )
))
c3 <- list(
    var2=list(
        conflicts=list(list(
            message="No good",
            resolution="But I fixed it already"
        )),
        metadata=list(
            name="Second"
        )
    ),
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
            name="First"
        )
    )
)
c4 <- c3
c4$var3 <- list(
    conflicts=list(list(
        message="Type mismatch"
    )),
    metadata=list(name="Last")
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
    
    expect_identical(formatConflicts(c1), "No conflicts.")
    expect_identical(formatConflicts(c2), 
        paste("Conflict: No good; Resolution: But I fixed it already; 1 variable:", dQuote("First")))
    expect_identical(formatConflicts(c3), 
        c(paste("Conflict: No good; Resolution: But I fixed it already; 2 variables:", dQuote("Second"), "and", dQuote("First")),
        paste("Conflict: Oh, and there was another problem; Resolution: But it's also cool; 1 variable:", dQuote("First"))))
})

source("conflicts.R")
test_that("Complex conflicts are formatted", {
    expect_identical(formatConflicts(mock.conflicts), 
        c(paste("Conflict: Only in existing dataset; Resolution: Additional rows will be marked missing.; 1 variable:", dQuote("mr_1")),
        paste("Conflict: Only in new dataset; Resolution: Variable will be added with existing rows marked missing.; 1 variable:", dQuote("mr_3")),
        paste("Conflict: Subvariables didn't match; Resolution: Union of subvariables will be used; 1 variable:", dQuote("MR"))))
})
