context("Crosstabbing")

cubedf <- df
cubedf$v7 <- as.factor(c(rep("C", 10), rep("D", 5), rep("E", 5)))

if (run.integration.tests) {
    with(test.authentication, {
        with(test.dataset(cubedf), {
            kube <- try(getCube(~ v7, data=ds))
            test_that("We can get a univariate categorical cube", {
                expect_true(inherits(kube, "CrunchCube"))
                expect_equivalent(cubeToArray(kube),
                    array(c(10, 5, 5), dim=c(3L),
                        dimnames=list(v7=LETTERS[3:5])))
                ## Not sure why not identical, str makes them look the same
            })
            
            kube <- try(getCube(~ v4 + v7, data=ds))
            test_that("We can get a bivariate categorical cube", {
                expect_true(inherits(kube, "CrunchCube"))
                expect_identical(cubeToArray(kube),
                    array(c(5, 5, 3, 2, 2, 3), dim=c(2L, 3L),
                        dimnames=list(v4=c("B", "C"), v7=LETTERS[3:5])))
            })
            
            test_that("prop.table on CrunchCube", {
                
            })
        })
    })
}
