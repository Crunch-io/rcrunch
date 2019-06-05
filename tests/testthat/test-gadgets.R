context("Shiny gadgets")

test_that("callFromOtherPackage", {
    NOTAFUNCTION <- sum <- function(x) {
        call <- match.call()
        callFromOtherPackage(call, "base")
    }
    expect_equal(sum(1:4), 10)
    expect_error(
        NOTAFUNCTION(1:4),
        "Please install the latest version of base to access NOTAFUNCTION"
    )
})

with_mock(
    `crunch::callFromOtherPackage` = function(call, pkg) {
        call <- capture.output(print(call))
        halt(pkg, "::", call, " called")
    }, {
        test_that("Gadget calling", {
            expect_error(makeArrayGadget(), "crunchy::makeArrayGadget() called", fixed = TRUE)
            expect_error(
                listDatasets(shiny = TRUE),
                "crunchy::listDatasetGadget(kind = kind, refresh = refresh) called",
                fixed = TRUE
            )
        })
    }
)
