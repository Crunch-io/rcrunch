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

test_that("attributeURL", {
    testds <- as.dataset(ds)
    expect_identical(attributeURL(testds, "name"), paste0(ds$self, "name"))
})

if (!run.only.local.tests) {
    test_that("refresh", {
        refresh_test <- df
        login(test.user)
            rt <- newDataset(refresh_test)
            expect_identical(rt, refresh(rt))
            rt2 <- rt
            rt2@body$name <- "something else"
            expect_false(identical(rt2, rt))
            expect_false(identical(rt2, refresh(rt2)))
            expect_identical(refresh(rt2), rt)
        logout()
    })
}