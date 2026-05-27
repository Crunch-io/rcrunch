test_that("typed_crunch_variable validation", {
    expect_snapshot_error(typed_crunch_variable(character(0), "NOT-A-TYPE", NULL))
})

test_that("Can get/set crunch variable attributes", {
    var <- new_crunch_variable(
        character(0),
        label = "lbl",
        description = "desc",
        notes = "not",
        path = "/Public"
    )

    # Can get
    expect_equal(label(var), "lbl")
    expect_equal(description(var), "desc")
    expect_equal(notes(var), "not")
    expect_equal(path(var), "/Public")

    # And set
    label(var) <- "lbl2"
    expect_equal(label(var), "lbl2")
    description(var) <- "desc2"
    expect_equal(description(var), "desc2")
    notes(var) <- "not2"
    expect_equal(notes(var), "not2")
    path(var) <- "/Public/v2"
    expect_equal(path(var), "/Public/v2")

    # And set optional ones to NULL
    description(var) <- NULL
    expect_equal(description(var), NULL)
    notes(var) <- NULL
    expect_equal(notes(var), NULL)
    path(var) <- NULL
    expect_equal(path(var), NULL)

})

test_that("Validation works during setting variable attributes", {
    var <- new_crunch_variable(
        character(0),
        label = "lbl",
        description = "desc",
        notes = "not",
        path = "/Public"
    )

    expect_snapshot_error(label(var) <- NULL)
    expect_snapshot_error(label(var) <- c("multiple", "strings"))
    expect_snapshot_error(description(var) <- 123)
    expect_snapshot_error(notes(var) <- ~abc)
    expect_snapshot_error(path(var) <- function(x) x + 1)
})
