mock_as_vector <- function(...) {
    data.frame(
        x1 = c(letters[1:2], NA, NA),
        x2 = c(letters[1:2], "a", NA),
        stringsAsFactors = TRUE
    )
}

with_mock_crunch({
    ds <- loadDataset("test ds")

    test_that("rowDistinct works (na.rm = TRUE)", {
        expect_equal(
            rowDistinct(ds$catarray, name = "x"),
            VarDef(
                as.integer(c(2, 2, 2, 1, 1, 2, 1, 2, 2, 1, 1, 2, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 2)), # nolint
                name = "x"
            )
        )
    })

    test_that("rowDistinct works (na.rm = FALSE)", {
        expect_equal(
            rowDistinct(ds$catarray, name = "x", na.rm = FALSE),
            VarDef(
                as.integer(c(2, 3, 2, 2, 2, 3, 2, 3, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 3, 1, 2, 1, 2, 2, 2)), # nolint
                name = "x"
            )
        )
    })

    test_that("error for array var subset for rowDistinct until #457", {
        expect_error(
            rowDistinct(ds$catarray[c("subvar2", "subvar1")], name = "x"),
            "`rowDistinct` does not yet support subsets of array subvariables"
        )
    })
    # test_that("rowDistinct works subset of array var subvars", {
    #     expect_equal(
    #         rowDistinct(ds$catarray[c("subvar2", "subvar1")], name = "x"),
    #         VarDef(
    #             as.integer(c(2, 2, 2, 0, 1, 1, 1, 2, 1, 1, 0, 2, 0, 0, 1, 1, 1, 2, 2, 1, 0, 1, 0, 1, 2)), # nolint
    #             name = "x"
    #         )
    #     )
    # })

    test_that("error for non-array var rowDistinct", {
        expect_error(rowDistinct(ds$textvar), "x must be an array variable")
    })

    test_that("straightlineResponse works", {
        expect_equal(
            straightlineResponse(ds$catarray, name = "x"),
            VarDef(
                ds$catarray$subvar1 == ds$catarray$subvar2 & ds$catarray$subvar3 == ds$catarray$subvar2, #nolint
                name = "x"
            )
        )
    })

    test_that("straightlineResponse works on subset of array var subvars", {
        expect_equal(
            straightlineResponse(ds$catarray[c("subvar2", "subvar1")], name = "x"),
            VarDef(
                ds$catarray$subvar1 == ds$catarray$subvar2,
                name = "x"
            )
        )
    })

    test_that("error for non-array var straightline", {
        expect_error(straightlineResponse(ds$textvar), "x must be an array variable")
    })

    test_that("error single var array straightline", {
        expect_error(
            straightlineResponse(ds$catarray["subvar1"]),
            "Array must have more than 1 subvariable."
        )
    })
})
