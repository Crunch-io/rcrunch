context("ZCL expressions")

test_that("r2zcl cases", {
    expect_identical(r2zcl(4), as.zcl(value = 4))
    expect_identical(r2zcl(c(4, 6)), as.zcl(column = c(4, 6)))
    expect_identical(r2zcl(I(4)), as.zcl(column = I(4)))
})

test_that("zcl(logical)", {
    z1 <- zcl(c(TRUE, NA))
    expect_identical(z1$type$class, "categorical")
})

with_mock_crunch({
    ds <- loadDataset("test ds")
    test_that("has.function", {
        func <- zfunc("cast", ds$birthyr, "text")
        expect_true(has.function(func, "cast"))
        expect_false(has.function(func, "case"))

        func <- zfunc(
            "case",
            zfunc("cast", ds$birthyr, "text"),
            list(args = list())
        )
        expect_true(has.function(func, "cast"))
        expect_true(has.function(func, "case"))
        expect_false(has.function(func, "cube_mean"))

        func <- zfunc("case", zfunc(
            "cast",
            zfunc("cube_mean", ds$birthyr),
            "text"
        ))
        expect_true(has.function(func, "cast"))
        expect_true(has.function(func, "case"))
        expect_true(has.function(func, "cube_mean"))
    })

    test_that("named and unnamed keywords passed correctly", {
        func <- zfunc(
            "alter_array",
            ds$mymrset,
            add = list(map = list("5" = list(variable = self(ds$gender))))
        )

        expect_equal(
            func,
            list(
                `function` = "alter_array",
                args = list(
                    list(variable = "https://app.crunch.io/api/datasets/1/variables/mymrset/")
                ),
                kwargs = list(
                    add = list(map = list(`5` = list(variable = "https://app.crunch.io/api/datasets/1/variables/gender/"))) # nolint
                )
            )
        )
    })

    test_that("zcl (VarDef)", {
        expect_equal(
            zcl(VarDef(ds$gender, name = "x")),
            list(
                variable = "https://app.crunch.io/api/datasets/1/variables/gender",
                references = list(name = "x")
            )
        )
    })
})
