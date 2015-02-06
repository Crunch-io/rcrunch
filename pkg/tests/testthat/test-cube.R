context("Crosstabbing")

cubedf <- df
cubedf$v7 <- as.factor(c(rep("C", 10), rep("D", 5), rep("E", 5)))
cubedf$v8 <- as.Date(0:1, origin="1955-11-05")

test_that("bin CrunchExpr", {
    x <- list(variable="test") ## "ZCL"
    expect_true(inherits(bin(x), "CrunchExpr"))
    expect_identical(zcl(bin(x)),
        list(`function`="bin", args=list(list(variable="test"))))
})

test_that("rollup CrunchExpr from zcl variable", {
    x <- list(variable="test") ## "ZCL"
    expect_true(inherits(rollup(x), "CrunchExpr"))
    expect_identical(zcl(rollup(x)),
        list(`function`="rollup", args=list(list(variable="test"),
            list(value=NULL))))
    
    expect_true(inherits(rollup(x, resolution="Y"), "CrunchExpr"))
    expect_identical(zcl(rollup(x, resolution="Y")),
        list(`function`="rollup", args=list(list(variable="test"),
            list(value="Y"))))
})

test_that("rollup CrunchExpr from DatetimeVariable", {
    v <- as.variable(structure(list(self="v/", 
        body=list(name="V", type="datetime", 
        resolution="D", view=list(rollup_resolution="M"))),
        class="shoji"),
        tuple=VariableTuple(body=list(id="v")))
    expect_identical(tuple(v), VariableTuple(body=list(id="v")))
    expect_identical(zcl(v), list(variable="v"))
    
    expect_true(inherits(rollup(v), "CrunchExpr"))
    expect_identical(zcl(rollup(v)),
        list(`function`="rollup", args=list(list(variable="v"),
            list(value="M"))))
    expect_true(inherits(rollup(v, resolution="Y"), "CrunchExpr"))
    expect_identical(zcl(rollup(v, resolution="Y")),
        list(`function`="rollup", args=list(list(variable="v"),
            list(value="Y"))))
    expect_true(inherits(rollup(v, resolution=NULL), "CrunchExpr"))
    expect_identical(zcl(rollup(v, resolution=NULL)),
        list(`function`="rollup", args=list(list(variable="v"),
            list(value=NULL))))
})

test_that("rollup resolution validation", {
    expect_error(rollup("a", resolution="Invalid"), 
        " is invalid. Valid values are NULL, ")
})

if (run.integration.tests) {
    with(test.authentication, {
        with(test.dataset(cubedf), {
            test_that("We can get a univariate categorical cube", {
                kube <- try(getCube(~ v7, data=ds))
                expect_true(inherits(kube, "CrunchCube"))
                expect_equivalent(cubeToArray(kube),
                    array(c(10, 5, 5), dim=c(3L),
                        dimnames=list(v7=LETTERS[3:5])))
                ## Not sure why not identical, str makes them look the same
            })
            
            test_that("We can get a bivariate categorical cube", {
                kube <- try(getCube(~ v4 + v7, data=ds))
                expect_true(inherits(kube, "CrunchCube"))
                expect_identical(cubeToArray(kube),
                    array(c(5, 5, 3, 2, 2, 3), dim=c(2L, 3L),
                        dimnames=list(v4=c("B", "C"), v7=LETTERS[3:5])))
            })
            
            is.na(categories(ds$v7)) <- "D"
            test_that("useNA on univariate cube", {
                expect_equivalent(cubeToArray(getCube(~ v7, data=ds)),
                    array(c(10, 5), dim=2L, dimnames=list(v7=c("C", "E"))))
                expect_equivalent(cubeToArray(getCube(~ v7, data=ds,
                    useNA="ifany")),
                    array(c(10, 5, 5), dim=c(3L),
                        dimnames=list(v7=LETTERS[3:5])))
                expect_equivalent(cubeToArray(getCube(~ v7, data=ds,
                    useNA="always")),
                    array(c(10, 5, 5), dim=c(3L),
                        dimnames=list(v7=LETTERS[3:5])))
            })
            test_that("useNA on bivariate cube", {
                expect_equivalent(cubeToArray(getCube(~ v4 + v7, data=ds)),
                    array(c(5, 5, 2, 3), dim=c(2L, 2L),
                        dimnames=list(v4=c("B", "C"), v7=c("C", "E"))))
                expect_equivalent(cubeToArray(getCube(~ v4 + v7, data=ds,
                    useNA="ifany")),
                    array(c(5, 5, 3, 2, 2, 3), dim=c(2L, 3L),
                        dimnames=list(v4=c("B", "C"), v7=LETTERS[3:5])))
                expect_equivalent(cubeToArray(getCube(~ v4 + v7, data=ds,
                    useNA="always")),
                    array(c(5, 5, 3, 2, 2, 3), dim=c(2L, 3L),
                        dimnames=list(v4=c("B", "C"), v7=LETTERS[3:5])))
            })
            
            test_that("univariate datetime cube", {
                kube <- try(getCube(~ v8, data=ds))
                expect_true(inherits(kube, "CrunchCube"))
                expect_equivalent(cubeToArray(kube),
                    array(c(10, 10), dim=c(2L),
                        dimnames=list(v8=c("1955-11-05", "1955-11-06"))))
            })
            test_that("bivariate cube with datetime", {
                expect_equivalent(cubeToArray(getCube(~ v8 + v7, data=ds)),
                    array(c(5, 5, 2, 3), dim=c(2L, 2L),
                        dimnames=list(v8=c("1955-11-05", "1955-11-06"),
                        v7=c("C", "E"))))
                expect_equivalent(cubeToArray(getCube(~ v8 + v7, data=ds,
                    useNA="ifany")),
                    array(c(5, 5, 3, 2, 2, 3), dim=c(2L, 3L),
                        dimnames=list(v8=c("1955-11-05", "1955-11-06"), 
                        v7=LETTERS[3:5])))
                expect_equivalent(cubeToArray(getCube(~ v8 + v7, data=ds,
                    useNA="always")),
                    array(c(5, 5, 3, 2, 2, 3), dim=c(2L, 3L),
                        dimnames=list(v8=c("1955-11-05", "1955-11-06"), 
                        v7=LETTERS[3:5])))
            })
            
            test_that("datetime rollup cubes", {
                ## Default rollup resolution for this should be same as
                ## its resolution, given the date range
                expect_equivalent(cubeToArray(getCube(~ rollup(v8) + v7,
                    data=ds)),
                    cubeToArray(getCube(~ v8 + v7, data=ds))) 
                expect_equivalent(cubeToArray(getCube(~ rollup(v8, "M") + v7,
                    data=ds)),
                    array(c(10, 5), dim=c(1L, 2L),
                        dimnames=list(v8=c("1955-11"),
                        v7=c("C", "E"))))
                expect_equivalent(cubeToArray(getCube(~ rollup(v8, "Y") + v7,
                    data=ds)),
                    array(c(10, 5), dim=c(1L, 2L),
                        dimnames=list(v8=c("1955"),
                        v7=c("C", "E"))))
            })
            
            test_that("univariate cube with binned numeric", {
                kube <- try(getCube(~ bin(v3), data=ds))
                expect_true(inherits(kube, "CrunchCube"))
                expect_equivalent(cubeToArray(kube),
                    array(c(2, 5, 5, 5, 3), dim=c(5L),
                        dimnames=list(v3=c("5-10", "10-15", "15-20", "20-25",
                        "25-30"))))
            })
            test_that("bivariate cube with binned numeric", {
                expect_equivalent(cubeToArray(getCube(~ bin(v3) + v7, data=ds)),
                    array(c(2, 5, 3, 0, 0,
                            0, 0, 0, 2, 3), dim=c(5L, 2L),
                        dimnames=list(v3=c("5-10", "10-15", "15-20", "20-25",
                        "25-30"),
                        v7=c("C", "E"))))
                expect_equivalent(cubeToArray(getCube(~ bin(v3) + v7, data=ds,
                    useNA="ifany")),
                    array(c(2, 5, 3, 0, 0,
                            0, 0, 2, 3, 0,
                            0, 0, 0, 2, 3), dim=c(5L, 3L),
                        dimnames=list(v3=c("5-10", "10-15", "15-20", "20-25",
                        "25-30"), 
                        v7=LETTERS[3:5])))
                expect_equivalent(cubeToArray(getCube(~ bin(v3) + v7, data=ds,
                    useNA="always")),
                    array(c(2, 5, 3, 0, 0,
                            0, 0, 2, 3, 0,
                            0, 0, 0, 2, 3), dim=c(5L, 3L),
                        dimnames=list(v3=c("5-10", "10-15", "15-20", "20-25",
                        "25-30"), 
                        v7=LETTERS[3:5])))
            })
            test_that("unbinned numeric", {
                expect_equivalent(cubeToArray(getCube(~ v1, data=ds)),
                    array(rep(1, 15), dim=15L, dimnames=list(v1=df$v1[6:20])))
                expect_equivalent(cubeToArray(getCube(~ v1, data=ds,
                    useNA="ifany")),
                    array(c(rep(1, 15), 5), dim=16L,
                        dimnames=list(v1=c(df$v1[6:20], "<NA>"))))
            })
            
            test_that("prop.table on CrunchCube", {
                
            })
        })
    })
}
