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

    test_that("error for non-array var straightline", {
        expect_error(straightlineResponse(ds$textvar), "x must be an array variable")
    })

    test_that("rowCount creates correct vardef", {
        expect_equal(
            rowCount(ds$mymrset, name = "row count"),
            VarDef(crunchdbFunc("selected_depth", ds$mymrset), name = "row count")
        )
    })
})
