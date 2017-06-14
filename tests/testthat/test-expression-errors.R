context("Expression validation")

with_mock_crunch({
    ds <- loadDataset("test ds")
    badexpr <- ds$NOTAVARIABLE == 3
    age <- 2016 - ds$birthyr

    test_that("Can't subset a dataset with an invalid expression", {
        expect_error(ds[ds$NOTAVARIABLE == 3,],
            "Invalid expression: ds$NOTAVARIABLE == 3", fixed=TRUE)
        expect_error(ds[badexpr,],
            "Invalid expression: badexpr", fixed=TRUE)
        expect_warning(
            expect_error(ds[!is.na(ds$NOTAVARIABLE),],
                "Invalid expression: !is.na(ds$NOTAVARIABLE)", fixed=TRUE),
            "is.na() applied to non-(list or vector) of type 'NULL'", fixed=TRUE)
        expect_error(ds[!duplicated(ds$NOTAVARIABLE),],
            "Invalid expression: !duplicated(ds$NOTAVARIABLE)", fixed=TRUE)
    })

    test_that("Can't subset a variable with an invalid expression", {
        expect_error(ds$gender[ds$NOTAVARIABLE == 3],
            "Invalid expression: ds$NOTAVARIABLE == 3", fixed=TRUE)
        expect_error(ds$gender[badexpr],
            "Invalid expression: badexpr", fixed=TRUE)
        expect_warning(
            expect_error(ds$gender[!is.na(ds$NOTAVARIABLE)],
                "Invalid expression: !is.na(ds$NOTAVARIABLE)", fixed=TRUE),
            "is.na() applied to non-(list or vector) of type 'NULL'", fixed=TRUE)
        expect_error(ds$gender[!duplicated(ds$NOTAVARIABLE)],
            "Invalid expression: !duplicated(ds$NOTAVARIABLE)", fixed=TRUE)
    })

    test_that("Can't subset a CrunchExpr with an invalid expression", {
        expect_error(age[ds$NOTAVARIABLE == 3],
            "Invalid expression: ds$NOTAVARIABLE == 3", fixed=TRUE)
        expect_error(age[badexpr],
            "Invalid expression: badexpr", fixed=TRUE)
        expect_warning(
            expect_error(age[!is.na(ds$NOTAVARIABLE)],
                "Invalid expression: !is.na(ds$NOTAVARIABLE)", fixed=TRUE),
            "is.na() applied to non-(list or vector) of type 'NULL'", fixed=TRUE)
        expect_error(age[!duplicated(ds$NOTAVARIABLE)],
            "Invalid expression: !duplicated(ds$NOTAVARIABLE)", fixed=TRUE)
    })

    test_that("Can't compose CrunchLogicalExprs with invalid", {
        expect_error(ds$gender %in% "Male" | ds$NOTAVARIABLE == 3,
            "Invalid expression (probably a reference to a variable that doesn't exist): ds$gender %in% \"Male\" | ds$NOTAVARIABLE == 3", fixed=TRUE)
        expect_error(ds$gender %in% "Male" & ds$NOTAVARIABLE == 3,
            "Invalid expression (probably a reference to a variable that doesn't exist): ds$gender %in% \"Male\" & ds$NOTAVARIABLE == 3", fixed=TRUE)
        expect_error(ds$NOTAVARIABLE == 3 | ds$gender %in% "Male",
            "Invalid expression (probably a reference to a variable that doesn't exist): ds$NOTAVARIABLE == 3 | ds$gender %in% \"Male\"", fixed=TRUE)
        expect_error(badexpr | ds$gender %in% "Male",
            "Invalid expression (probably a reference to a variable that doesn't exist): badexpr | ds$gender %in% \"Male\"", fixed=TRUE)
        expect_error(!(ds$NOTAVARIABLE == 3) | ds$gender %in% "Male",
            "Invalid expression (probably a reference to a variable that doesn't exist): !(ds$NOTAVARIABLE == 3) | ds$gender %in% \"Male\"", fixed=TRUE)
        expect_error(with(ds, NOTAVARIABLE == 3 | gender %in% "Male"),
            "object 'NOTAVARIABLE' not found") ## Base R error
        expect_error(ds$NOTAVARIABLE %in% 3 | ds$gender %in% "Male",
            "Invalid expression (probably a reference to a variable that doesn't exist): ds$NOTAVARIABLE %in% 3 | ds$gender %in% \"Male\"", fixed=TRUE)
        expect_warning(
            expect_error(is.na(ds$NOTAVARIABLE) | ds$gender %in% "Male",
                "Invalid expression (probably a reference to a variable that doesn't exist): is.na(ds$NOTAVARIABLE) | ds$gender %in% \"Male\"",
                fixed=TRUE),
            "is.na() applied to non-(list or vector) of type 'NULL'", fixed=TRUE)
    })
})
