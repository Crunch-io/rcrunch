context("Basic cube methods")

v7 <- v7_ifany <- v7_always <- loadCube("cubes/univariate-categorical.json")
v7_ifany@useNA <- "ifany"
v7_always@useNA <- "always"
v4_x_v7 <- v4_x_v7_ifany <- v4_x_v7_always <- loadCube("cubes/cat-x-cat.json")
v4_x_v7_ifany@useNA <- "ifany"
v4_x_v7_always@useNA <- "always"

test_that("useNA on univariate cube", {
    expect_equal(as.array(v7),
        cubify(10, 5,
            dims=list(
                v7=c("C", "E")
            )))
    expect_equal(as.array(v7_ifany),
        cubify(10, 5, 5,
            dims=list(
                v7=c("C", "D", "E")
            )))
    expect_equal(as.array(v7_always),
        cubify(10, 5, 5, 0,
            dims=list(
                v7=c(LETTERS[3:5], "No Data")
            )))
})

test_that("useNA on bivariate cube", {
    expect_equivalent(as.array(v4_x_v7),
        cubify(
                5, 2,
                5, 3,
            dims=list(
                v4=c("B", "C"),
                v7=c("C", "E")
            )))
    expect_equivalent(as.array(v4_x_v7_ifany),
        cubify(
                5, 3, 2,
                5, 2, 3,
            dims=list(
                v4=c("B", "C"),
                v7=c(LETTERS[3:5])
            )))
    expect_equivalent(as.array(v4_x_v7_always),
        cubify(
                5, 3, 2, 0,
                5, 2, 3, 0,
                0, 0, 0, 0,
            dims=list(
                v4=c("B", "C", "No Data"),
                v7=c(LETTERS[3:5], "No Data")
            )))
})

test_that("Cube print method", {
    expect_output(v4_x_v7_ifany,
        paste("   v7",
              "v4  C D E",
              "  B 5 3 2",
              "  C 5 2 3", sep="\n"))
})

with_mock_crunch({
    ds <- loadDataset("test ds")

    test_that("useNA is set correctly on CrunchCube init", {
        expect_identical(crtabs(~ birthyr > 1980, data=ds)@useNA,
            "no")
        expect_identical(crtabs(~ birthyr > 1980, data=ds, useNA="ifany")@useNA,
            "ifany")
        expect_identical(crtabs(~ birthyr > 1980, data=ds, useNA="always")@useNA,
            "always")
        expect_error(crtabs(~ birthyr > 1980, data=ds, useNA="fosho"),
            "should be one of")
    })
})
