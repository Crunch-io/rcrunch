context("Crosstabbing")

cubedf <- df
cubedf$v7 <- as.factor(c(rep("C", 10), rep("D", 5), rep("E", 5)))
cubedf$v8 <- as.Date(0:1, origin="1955-11-05")

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
            
            with(test.dataset(mrdf, "mrds"), {
                mrds <- mrdf.setup(mrds, selections="1.0")
                test_that("univariate multiple response cube", {
                    kube <- try(getCube(~ MR, data=mrds))
                    expect_true(inherits(kube, "CrunchCube"))
                    expect_equivalent(cubeToArray(kube),
                        array(c(2, 1, 1), dim=c(3L),
                            dimnames=list(MR=c("mr_1", "mr_2", "mr_3"))))
                })
                
                test_that("bivariate cube with MR", {
                    kube <- try(getCube(~ MR + v4, data=mrds))
                    expect_true(inherits(kube, "CrunchCube"))
                    expect_equivalent(cubeToArray(kube),
                        array(c(2, 1, 1, 0, 0, 0), dim=c(3L, 2L),
                            dimnames=list(MR=c("mr_1", "mr_2", "mr_3"),
                            v4=c("B", "C"))))
                    
                    kube <- try(getCube(~ v4 + MR, data=mrds))
                    expect_true(inherits(kube, "CrunchCube"))
                    expect_equivalent(cubeToArray(kube),
                        array(c(2, 0, 1, 0, 1, 0), dim=c(2L, 3L),
                            dimnames=list(v4=c("B", "C"),
                            MR=c("mr_1", "mr_2", "mr_3"))))
                    
                    kube <- try(getCube(~ v4 + MR, data=mrds, useNA="ifany"))
                    expect_true(inherits(kube, "CrunchCube"))
                    print(kube)
                    expect_equivalent(cubeToArray(kube),
                        array(c(2, 0, 1, 0, 1, 0, 0, 1), dim=c(2L, 4L),
                            dimnames=list(v4=c("B", "C"),
                            MR=c("mr_1", "mr_2", "mr_3", "<NA>"))))
                })
            })
            
            test_that("prop.table on CrunchCube", {
                
            })
        })
    })
}
