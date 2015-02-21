context("Cubes with categorical array and multiple response")

if (run.integration.tests) {
    with(test.authentication, {
        with(test.dataset(mrdf, "mrds"), {
            mrds <- mrdf.setup(mrds, selections="1.0")
            test_that("univariate multiple response cube", {
                kube <- try(crtabs(~ MR, data=mrds))
                expect_true(inherits(kube, "CrunchCube"))
                expect_equivalent(as.array(kube),
                    array(c(2, 1, 1), dim=c(3L),
                        dimnames=list(MR=c("mr_1", "mr_2", "mr_3"))))
            })
            
            test_that("bivariate cube with MR", {
                kube <- try(crtabs(~ MR + v4, data=mrds))
                expect_true(inherits(kube, "CrunchCube"))
                expect_equivalent(as.array(kube),
                    array(c(2, 1, 1, 0, 0, 0), dim=c(3L, 2L),
                        dimnames=list(MR=c("mr_1", "mr_2", "mr_3"),
                        v4=c("B", "C"))))
                
                kube <- try(crtabs(~ v4 + MR, data=mrds))
                expect_true(inherits(kube, "CrunchCube"))
                expect_equivalent(as.array(kube),
                    array(c(2, 0, 1, 0, 1, 0), dim=c(2L, 3L),
                        dimnames=list(v4=c("B", "C"),
                        MR=c("mr_1", "mr_2", "mr_3"))))
                
                kube <- try(crtabs(~ v4 + MR, data=mrds, useNA="ifany"))
                expect_true(inherits(kube, "CrunchCube"))
                expect_equivalent(as.array(kube),
                    array(c(2, 0, 1, 0, 1, 0, 0, 1), dim=c(2L, 4L),
                        dimnames=list(v4=c("B", "C"),
                        MR=c("mr_1", "mr_2", "mr_3", "<NA>"))))
            })
            
            mrds$MR <- undichotomize(mrds$MR)
            alias(mrds$MR) <- "CA"
            name(mrds$CA) <- "Cat array"
            test_that("'univariate' categorical array cube", {
                kube <- try(crtabs(~ CA, data=mrds, useNA="ifany"))
                expect_true(inherits(kube, "CrunchCube"))
                expect_equivalent(as.array(kube), 
                    array(c(1, 2, 2, 2, 1, 1, 1, 1, 1),
                    dim=c(3L, 3L),
                    dimnames=list(CA=c("mr_1", "mr_2", "mr_3"),
                        CA=c("0.0", "1.0", "<NA>"))))
            })
        })
    })
}