context("Shoji")

test_that("is.shoji", {
    fo <- list(element=1, self=2, description=3)
    expect_false(is.shoji(fo))
    expect_true(is.shoji.like(fo))
    class(fo) <- "shoji"
    expect_true(is.shoji(fo))
})

test_that("ShojiObject init and is", {
    expect_true(is.shojiObject(ShojiObject(element=1, self=2, description=3)))
    expect_false(is.shojiObject(5))
    fo <- list(element=1, self=2, description=3)
    class(fo) <- "shoji"
    expect_false(is.shojiObject(fo))
})

test_that("shoji S3 to ShojiObject", {
    fo <- list(element=1, self=2, description=3)
    class(fo) <- "shoji"
    expect_true(is.shojiObject(as.shojiObject(fo)))
})