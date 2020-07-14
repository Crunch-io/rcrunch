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

    test_that("tieredVar creates correct vardef", {
        expect_equal(
            tieredVar(ds$catarray, c(2, 1), name = "tiered var"),
            VarDef(crunchdbFunc("tiered", ds$catarray, list(value = I(c(2, 1)))), name = "tiered var") #nolint
        )

        expect_error(
            tieredVar(ds$birthyr, c(2, 1), name = "tiered var"),
            "must be of type 'Array' for tiered"
        )

        expect_error(
            tieredVar(ds$catarray, c(3, 1), name = "tiered var"),
            "3 in id"
        )
    })

    test_that("tieredVar type specifications work", {
        expect_equal(
            tieredVar(ds$catarray, c(2, 1), name = "tiered var"),
            tieredVar(ds$catarray, c("B", "A"), name = "tiered var")
        )

        expect_equal(
            tieredVar(ds$catarray, c(value = 1, value = 0), name = "tiered var"),
            tieredVar(ds$catarray, c("B", "A"), name = "tiered var")
        )
    })

    test_that("rowCount creates correct vardef", {
        expect_equal(
            rowCount(ds$mymrset, name = "row count"),
            VarDef(crunchdbFunc("selected_depth", ds$mymrset), name = "row count")
        )
    })

    test_that("rowAny creates correct vardef", {
        expect_equal(
            rowAny(ds$mymrset, name = "row any"),
            VarDef(crunchdbFunc("any", ds$mymrset), name = "row any")
        )
    })

    test_that("rowAll creates correct vardef", {
        expect_equal(
            rowAll(ds$mymrset, name = "row all"),
            VarDef(crunchdbFunc("all", ds$mymrset), name = "row all")
        )
    })

    test_that("rowAnyNA creates correct vardef", {
        expect_equal(
            rowAnyNA(ds$mymrset, name = "row any missing"),
            VarDef(crunchdbFunc("any_missing", ds$mymrset), name = "row any missing")
        )
    })
})
