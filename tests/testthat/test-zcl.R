context("ZCL expressions")

test_that("r2zcl cases", {
    expect_identical(r2zcl(4), as.zcl(value=4))
    expect_identical(r2zcl(c(4, 6)), as.zcl(column=c(4, 6)))
    expect_identical(r2zcl(I(4)), as.zcl(column=I(4)))
})

test_that("zcl(logical)", {
    z1 <- zcl(c(TRUE, NA))
    expect_identical(z1$type$class, "boolean")
    with(temp.option(crunch.3vl=TRUE), {
        z2 <- zcl(c(TRUE, NA))
        expect_identical(z2$type$class, "categorical")
    })
})
