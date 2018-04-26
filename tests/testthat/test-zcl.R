context("ZCL expressions")

test_that("r2zcl cases", {
    expect_identical(r2zcl(4), as.zcl(value=4))
    expect_identical(r2zcl(c(4, 6)), as.zcl(column=c(4, 6)))
    expect_identical(r2zcl(I(4)), as.zcl(column=I(4)))
})
