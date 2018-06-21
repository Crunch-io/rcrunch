context("'Base sizes' in cubes")

all.dims <- list(
    Admit=c("Admitted", "Rejected"),
    Gender=c("Male", "Female")
)

cube <- loadCube("cubes/admit-by-gender-weighted.json")
b <- cubify(
    1198, 557,
    1493, 1278,
    dims = all.dims)
test_that("'bases' can be accessed and all margins work", {
    expect_identical(
        round(cube),
        cubify(
            1170, 547,
            1474, 1261,
            dims = all.dims))
    expect_identical(bases(cube, 0), b)
    expect_identical(bases(cube, 1), margin.table(b, 1))
    expect_identical(bases(cube, 2), margin.table(b, 2))
    expect_identical(bases(cube), sum(b))
})

with_mock_crunch({
    ds <- loadDataset("test ds")
    m <- multitables(ds)[[1]]
    with_POST("https://app.crunch.io/api/datasets/1/multitables/apidocs-tabbook/", {
        book1 <- tabBook(m, data=ds, format="json")
    })

    test_that("bases methods exist for TabBookResult and MultitableResult", {
        expect_identical(
            bases(book1, 0)[[1]][[2]],
            cubify(
                4, 0, 1,
                0, 5, 3,
                1, 3, 5,
                dims = list(
                    allpets = c("Cat", "Dog", "Bird"),
                    allpets = c("Cat", "Dog", "Bird"))
            ))
        expect_identical(
            bases(book1)[[1]][[2]],
            cubify(
                17,
                dims = list(
                    allpets = c("Cat", "Dog", "Bird"),
                    allpets = c("Cat", "Dog", "Bird"))
            ))
    })

    test_that("base for univariate stats", {
        expect_equal(bases(crtabs(max(birthyr) ~ 1, data=ds)), 25)
        expect_error(bases(crtabs(max(birthyr) ~ 1, data=ds), 1),
            "Margin 1 exceeds Cube's number of dimensions (0)", fixed=TRUE)
    })
})
