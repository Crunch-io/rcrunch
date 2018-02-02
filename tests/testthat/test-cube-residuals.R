context("cube residual helpers")

test_that("broadcast broadcasts a vector", {
    expect_equal(broadcast(c(1, 2, 3), ncol = 2),
                 array(c(1, 2, 3, 1, 2, 3), dim = c(3, 2)))
    expect_equal(broadcast(c(1, 2, 3), nrow = 2),
                 array(c(1, 1, 2, 2, 3, 3), dim = c(2, 3)))

    expect_equal(broadcast(c(1, 2, 3), dims = c(3, 2)),
                 array(c(1, 2, 3, 1, 2, 3), dim = c(3, 2)))
    expect_equal(broadcast(c(1, 2, 3), dims = c(2, 3)),
                 array(c(1, 1, 2, 2, 3, 3), dim = c(2, 3)))
})

test_that("broadcast returns a matrix that mataches", {
    array <- array(c(1, 2, 3, 1, 2, 3), dim = c(3, 2))
    expect_equal(broadcast(array, dims = c(3, 2)), array)
    expect_error(broadcast(array, dims = c(2, 3)),
                 paste0("Something has gone wrong broadcasting the vector ",
                 dQuote("array"), " to the dimensions c(2, 3)"),
                 fixed=TRUE)
})
