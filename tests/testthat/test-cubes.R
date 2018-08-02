context("Crosstabbing")

cubedf <- df
cubedf$v7 <- as.factor(c(rep("C", 10), rep("D", 5), rep("E", 5)))
cubedf$v8 <- as.Date(0:1, origin = "1955-11-05")

test_that("bin CrunchExpr", {
    x <- list(variable = "test") ## "ZCL"
    expect_is(bin(x), "CrunchExpr")
    expect_identical(
        zcl(bin(x)),
        list(`function` = "bin", args = list(list(variable = "test")))
    )
})

test_that("rollup CrunchExpr from zcl variable", {
    x <- list(variable = "test") ## "ZCL"
    expect_is(rollup(x), "CrunchExpr")
    expect_identical(
        zcl(rollup(x)),
        list(`function` = "rollup", args = list(
            list(variable = "test"),
            list(value = NULL)
        ))
    )

    expect_is(rollup(x, resolution = "Y"), "CrunchExpr")
    expect_identical(
        zcl(rollup(x, resolution = "Y")),
        list(`function` = "rollup", args = list(
            list(variable = "test"),
            list(value = "Y")
        ))
    )
})

test_that("rollup resolution validation", {
    expect_error(
        rollup("a", resolution = "Invalid"),
        " is invalid. Valid values are "
    )
    expect_error(
        rollup("a", resolution = 42),
        " is invalid. Valid values are "
    )
})

test_that("cube missing functions set @useNA", {
    cube <- loadCube(test_path("cubes/cat-x-mr-x-mr.json"))
    expect_equal(cube@useNA, "no")
    cube <- showMissing(cube)
    expect_equal(cube@useNA, "always")
    expect_equal(hideMissing(cube)@useNA, "no")
})

with_mock_crunch({
    ds <- loadDataset("test ds")
    v <- ds$starttime

    test_that("rollup CrunchExpr from DatetimeVariable", {
        expect_is(rollup(v), "CrunchExpr")
        expect_identical(
            zcl(rollup(v)),
            list(
                `function` = "rollup",
                args = list(
                    list(variable = "https://app.crunch.io/api/datasets/1/variables/starttime/"),
                    list(value = "s")
                )
            )
        )
        expect_is(rollup(v, resolution = "Y"), "CrunchExpr")
        expect_identical(
            zcl(rollup(v, resolution = "Y")),
            list(
                `function` = "rollup",
                args = list(
                    list(variable = "https://app.crunch.io/api/datasets/1/variables/starttime/"),
                    list(value = "Y")
                )
            )
        )
        expect_is(rollup(v, resolution = NULL), "CrunchExpr")
        expect_identical(
            zcl(rollup(v, resolution = NULL)),
            list(
                `function` = "rollup",
                args = list(
                    list(variable = "https://app.crunch.io/api/datasets/1/variables/starttime/"),
                    list(value = NULL)
                )
            )
        )
    })

    test_that("formulaToCubeQuery", {
        expect_identical(
            formulaToCubeQuery(mean(birthyr) ~ gender, data = ds),
            list(
                dimensions = list(zcl(ds$gender)),
                measures = list(mean = zfunc("cube_mean", ds$birthyr))
            )
        )
        expect_identical(
            formulaToCubeQuery(~gender, data = ds),
            list(
                dimensions = list(zcl(ds$gender)),
                measures = list(count = zfunc("cube_count"))
            )
        )
        expect_identical(
            formulaToCubeQuery(n() ~ gender, data = ds),
            list(
                dimensions = list(zcl(ds$gender)),
                measures = list(count = zfunc("cube_count"))
            )
        )
        expect_identical(
            formulaToCubeQuery(list(mean(birthyr), n()) ~ gender, data = ds),
            list(
                dimensions = list(zcl(ds$gender)),
                measures = list(
                    mean = zfunc("cube_mean", ds$birthyr),
                    count = zfunc("cube_count")
                )
            )
        )
    })
    test_that("formulaToCubeQuery preserves measure names", {
        expect_identical(
            formulaToCubeQuery(list(avg = mean(birthyr), cts = n()) ~ gender, data = ds),
            list(
                dimensions = list(zcl(ds$gender)),
                measures = list(
                    avg = zfunc("cube_mean", ds$birthyr),
                    cts = zfunc("cube_count")
                )
            )
        )
    })
})

adims <- CubeDims(list(
    v4 = list(
        name = c("B", "C"),
        missing = rep(FALSE, 2),
        references = list(name = "v4", alias = "v4", type = "categorical")
    ),
    v7 = list(
        name = c("C", "D", "E", "No Data"),
        missing = c(rep(FALSE, 3), TRUE),
        references = list(name = "v7", alias = "v7", type = "categorical")
    )
))
a1 <- CrunchCube(
    arrays = list("count" = array(c(
        8, 6,
        3, 2,
        2, 3,
        0, 0
    ), dim = c(2L, 4L))),
    dims = adims
)
#    v7
# v4  C D E No Data
#   B 8 3 2 0
#   C 6 2 3 0

df.dims <- list(
    v3 = c("5-10", "10-15", "15-20", "20-25", "25-30"),
    v4 = c("B", "C"),
    v7 = LETTERS[3:5],
    v8 = c("1955-11-05", "1955-11-06")
)

arrayify <- function(data, dims) {
    ## dims are names (aliases) of dims defined above
    dn <- df.dims[dims]
    array(data, dim = vapply(dn, length, integer(1), USE.NAMES = FALSE), dimnames = dn)
}

test_that("simple margin.table", {
    expect_equivalent(margin.table(a1, 1), margin.table(a1@arrays[[1]], 1))
    expect_identical(
        margin.table(a1, 1),
        cubify(13, 11, dims = df.dims["v4"])
    )
    expect_identical(
        margin.table(a1, 2),
        cubify(14, 5, 5, dims = df.dims["v7"])
    )
    expect_equivalent(margin.table(a1), margin.table(a1@arrays[[1]]))
    expect_identical(margin.table(a1), 24)
})

test_that("margin.table with missing", {
    a2 <- a1
    a2@dims[[2]]$missing[2] <- TRUE ## "D"
    expect_identical(a2@useNA, "no") ## The default.
    expect_identical(
        margin.table(a2, 1),
        cubify(10, 9, dims = df.dims["v4"])
    )
    expect_identical(
        margin.table(a2, 2),
        cubify(14, 5, dims = list(v7 = c("C", "E")))
    )
    expect_identical(margin.table(a2), 19)

    a2@useNA <- "ifany"
    ## Should be the same as first tests
    expect_identical(
        margin.table(a2, 1),
        cubify(13, 11, dims = df.dims["v4"])
    )
    expect_identical(
        margin.table(a2, 2),
        cubify(14, 5, 5, dims = df.dims["v7"])
    )
    expect_identical(margin.table(a2), 24)

    a2@useNA <- "always"
    expect_identical(
        margin.table(a2, 1),
        cubify(13, 11, dims = df.dims["v4"])
    )
    expect_identical(
        margin.table(a2, 2),
        cubify(14, 5, 5, 0, dims = list(v7 = c(LETTERS[3:5], "No Data")))
    )
    expect_identical(margin.table(a2), 24)
})

with_test_authentication({
    ds <- newDataset(cubedf)
    test_that("cubedf setup", {
        expect_identical(
            names(categories(ds$v7)),
            c("C", "D", "E", "No Data")
        )
    })
    test_that("We can get a univariate categorical cube", {
        kube <- crtabs(~v7, data = ds)
        expect_is(kube, "CrunchCube")
        expect_equal(as.array(kube), cubify(10, 5, 5, dims = df.dims["v7"]))
        ## Not sure why not identical, str makes them look the same
    })

    test_that("We can get a bivariate categorical cube", {
        kube <- crtabs(~v4 + v7, data = ds)
        expect_is(kube, "CrunchCube")
        expect_identical(
            as.array(kube),
            cubify(
                5, 3, 2,
                5, 2, 3,
                dims = list(
                    v4 = c("B", "C"),
                    v7 = c(LETTERS[3:5])
                )
            )
        )
    })

    ## Make a category with data be missing
    is.na(categories(ds$v7)) <- "D"
    ## Update it in the dimensions map
    df.dims$v7 <- c("C", "E")

    test_that("univariate datetime cube", {
        kube <- crtabs(~v8, data = ds)
        expect_is(kube, "CrunchCube")
        expect_equivalent(as.array(kube), arrayify(c(10, 10), "v8"))
    })
    test_that("bivariate cube with datetime", {
        expect_equivalent(
            as.array(crtabs(~v8 + v7, data = ds)),
            arrayify(c(5, 5, 2, 3), c("v8", "v7"))
        )
        expect_equivalent(
            as.array(crtabs(~v8 + v7, data = ds, useNA = "ifany")),
            array(c(5, 5, 3, 2, 2, 3),
                dim = c(2L, 3L),
                dimnames = list(
                    v8 = c("1955-11-05", "1955-11-06"),
                    v7 = LETTERS[3:5]
                )
            )
        )
        expect_equivalent(
            as.array(crtabs(~v8 + v7, data = ds, useNA = "always")),
            array(c(
                5, 5,
                3, 2,
                2, 3,
                0, 0
            ),
            dim = c(2L, 4L),
            dimnames = list(
                v8 = c("1955-11-05", "1955-11-06"),
                v7 = c(LETTERS[3:5], "No Data")
            )
            )
        )
    })

    test_that("datetime rollup cubes", {
        ## Default rollup resolution for this should be same as
        ## its resolution, given the date range
        expect_equivalent(
            as.array(crtabs(~rollup(v8) + v7, data = ds)),
            as.array(crtabs(~v8 + v7, data = ds))
        )
        expect_equivalent(
            as.array(crtabs(~rollup(v8, "M") + v7, data = ds)),
            array(c(10, 5),
                dim = c(1L, 2L),
                dimnames = list(
                    v8 = "1955-11",
                    v7 = c("C", "E")
                )
            )
        )
        expect_equivalent(
            as.array(crtabs(~rollup(v8, "Y") + v7, data = ds)),
            array(c(10, 5),
                dim = c(1L, 2L),
                dimnames = list(
                    v8 = "1955",
                    v7 = c("C", "E")
                )
            )
        )
    })

    test_that("univariate cube with binned numeric", {
        kube <- crtabs(~bin(v3), data = ds)
        expect_is(kube, "CrunchCube")
        expect_equivalent(
            as.array(kube),
            arrayify(c(2, 5, 5, 5, 3), "v3")
        )
    })
    test_that("bivariate cube with binned numeric", {
        expect_equivalent(
            as.array(crtabs(~bin(v3) + v7, data = ds)),
            arrayify(c(
                2, 5, 3, 0, 0,
                0, 0, 0, 2, 3
            ), c("v3", "v7"))
        )
        expect_equivalent(
            as.array(crtabs(~bin(v3) + v7, data = ds, useNA = "ifany")),
            array(c(
                2, 5, 3, 0, 0,
                0, 0, 2, 3, 0,
                0, 0, 0, 2, 3
            ),
            dim = c(5L, 3L),
            dimnames = list(
                v3 = c("5-10", "10-15", "15-20", "20-25", "25-30"),
                v7 = c("C", "D", "E")
            )
            )
        )
        expect_equivalent(
            as.array(crtabs(~bin(v3) + v7, data = ds, useNA = "always")),
            array(c(
                2, 5, 3, 0, 0,
                0, 0, 2, 3, 0,
                0, 0, 0, 2, 3,
                0, 0, 0, 0, 0
            ),
            dim = c(5L, 4L),
            dimnames = list(
                v3 = c("5-10", "10-15", "15-20", "20-25", "25-30"),
                v7 = c(LETTERS[3:5], "No Data")
            )
            )
        )
    })
    test_that("unbinned numeric", {
        expect_equivalent(
            as.array(crtabs(~v1, data = ds)),
            array(rep(1, 15), dim = 15L, dimnames = list(v1 = df$v1[6:20]))
        )
        expect_equivalent(
            as.array(crtabs(~v1, data = ds, useNA = "ifany")),
            array(c(rep(1, 15), 5),
                dim = 16L,
                dimnames = list(v1 = c(df$v1[6:20], "<NA>"))
            )
        )
    })

    test_that("Weighted cubes", {
        weight(ds) <- ds$v3
        expect_equivalent(
            as.array(crtabs(~v8 + v7, data = ds)),
            arrayify(c(60, 65, 50, 75), c("v8", "v7"))
        )
        expect_equivalent(
            as.array(crtabs(~v8 + v7, data = ds, weight = NULL)),
            arrayify(c(5, 5, 2, 3), c("v8", "v7"))
        )
        weight(ds) <- NULL
        expect_equivalent(
            as.array(crtabs(~v8 + v7, data = ds)),
            arrayify(c(5, 5, 2, 3), c("v8", "v7"))
        )
        expect_equivalent(
            as.array(crtabs(~v8 + v7, data = ds, weight = ds$v3)),
            arrayify(c(60, 65, 50, 75), c("v8", "v7"))
        )
    })

    test_that("Numeric aggregates", {
        expect_equivalent(
            as.array(crtabs(mean(v3) ~ v8 + v7, data = ds)),
            arrayify(c(12, 13, 25, 25), c("v8", "v7"))
        )
        expect_equivalent(
            as.array(crtabs(sum(v3) ~ v8 + v7, data = ds)),
            arrayify(c(60, 65, 50, 75), c("v8", "v7"))
        )
        expect_equivalent(
            as.array(crtabs(min(v3) ~ v8 + v7, data = ds)),
            arrayify(c(8, 9, 24, 23), c("v8", "v7"))
        )
        expect_equivalent(
            as.array(crtabs(median(v3) ~ v8 + v7, data = ds)),
            arrayify(c(12, 13, 25, 25), c("v8", "v7"))
        )
    })

    test_that("Numeric aggregates on categoricals with numeric values", {
        expect_equivalent(
            as.array(crtabs(mean(v4) ~ v4, data = ds)),
            arrayify(c(1, 2), "v4")
        )
    })

    test_that("Missing values in cubes", {
        expect_equivalent(
            round(as.array(crtabs(sd(v3) ~ bin(v3) + v7,
                data = ds
            )), 3),
            arrayify(c(
                0.707, 1.581, 1, NaN, NaN,
                NaN, NaN, NaN, 0.707, 1
            ), c("v3", "v7"))
        )
    })

    test_that("round cubes", {
        expect_equivalent(
            round(crtabs(sd(v3) ~ bin(v3) + v7, data = ds), 3),
            arrayify(c(
                0.707, 1.581, 1, NaN, NaN,
                NaN, NaN, NaN, 0.707, 1
            ), c("v3", "v7"))
        )
    })

    test_that("Cube with variables and R objects", {
        skip("object 'd4' not found")
        d4 <- cubedf$v4
        expect_equivalent(
            as.array(crtabs(~d4 + v7, data = ds)),
            arrayify(c(5, 5, 2, 3), c("v4", "v7"))
        )
    })

    test_that("Cube with transformations", {
        expect_equivalent(
            as.array(crtabs(~bin(v3 + 5), data = ds)),
            arrayify(c(2, 5, 5, 5, 3), "v3")
        )
    })

    test_that("prop.table on univariate cube", {
        expect_equivalent(
            prop.table(crtabs(~bin(v3 + 5), data = ds)),
            arrayify(c(2, 5, 5, 5, 3) / 20, "v3")
        )
    })

    test_that("prop.table on crosstab", {
        expect_equivalent(
            prop.table(crtabs(~bin(v3) + v7, data = ds)),
            arrayify(c(
                2, 5, 3, 0, 0,
                0, 0, 0, 2, 3
            ) / 15, c("v3", "v7"))
        )
        expect_equivalent(
            prop.table(crtabs(~bin(v3) + v7, data = ds), margin = 1),
            arrayify(c(
                1, 1, 1, 0, 0,
                0, 0, 0, 1, 1
            ), c("v3", "v7"))
        )
        expect_equivalent(
            prop.table(crtabs(~bin(v3) + v7, data = ds), margin = 2),
            arrayify(c(
                .2, .5, .3, 0, 0,
                0, 0, 0, .4, .6
            ), c("v3", "v7"))
        )
    })

    test_that("Univariate stats", {
        expect_equivalent(as.array(crtabs(mean(v3) ~ 1, data = ds)), 17.5)
    })
})
