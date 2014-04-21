context("Shoji")

test_that("is.shoji", {
    fo <- list(element="shoji:view", self=2, description=3)
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
    expect_true(is.shojiObject(ShojiObject(element=1, self=2, description=3, foo=4, junk=5)))
    sh <- ShojiObject(element=1, self=2, description=3, foo=4, junk=5)
    expect_identical(sh@self, 2)
})

test_that("shoji S3 to ShojiObject", {
    fo <- list(element=1, self=2, description=3)
    class(fo) <- "shoji"
    expect_true(is.shojiObject(as.shojiObject(fo)))
    expect_true(is.shojiObject(as.shojiObject(structure(list(element=1, self=2, description=3, foo=4, junk=5), class="shoji"))))
})

test_that("ShojiCatalog", {
    fo <- list(element=1, self=2, description=3, index=list(a=4, b=5))
    class(fo) <- "shoji"
    sho <- as.shojiObject(fo)
    expect_false(is.shojiCatalog(sho))
    expect_error(sho@index)
    fo$element <- "shoji:catalog"
    sho <- as.shojiObject(fo)
    expect_true(is.shojiCatalog(sho))
    expect_identical(sho@index, fo$index)
    
    fo2 <- fo
    fo2$index <- fo2$index[c(2,1)]
    expect_identical(as.shojiObject(fo), as.shojiObject(fo2))
    
})

if (!run.only.local.tests) {
    with(test.authentication, {
        with(test.dataset(df, "refresh_test"), {
            test_that("refresh", {
                rt <- .setup
                expect_identical(rt, refresh(rt))
                rt2 <- rt
                rt2@body$name <- "something else"
                expect_false(identical(rt2, rt))
                expect_false(identical(rt2, refresh(rt2)))
                expect_identical(refresh(rt2), rt)
            })
        })
    })
}