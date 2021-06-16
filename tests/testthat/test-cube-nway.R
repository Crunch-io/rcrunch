context("Cubes with >2 dimensions")

with_test_authentication({
    with(test.dataset(newDatasetFromFixture("apidocs")), {
        names(categories(ds$q1)) <- c(LETTERS[1:5], "No Data") ## To distinguish from other vars
        test_that("Cat x datetime x subvar", {
            kube <- crtabs(~ q1 + wave + petloc$petloc_home, data = ds)
            expect_equivalent(
                as.array(kube)[, , "Cat"],
                array(c(
                    1, 1, 0,
                    2, 0, 0
                ),
                dim = c(3L, 2L),
                dimnames = list(
                    q1 = c("A", "B", "C"),
                    wave = c("2014-12-01", "2015-01-01")
                )
                )
            )
            expect_equivalent(
                as.array(kube)[, , "Dog"],
                array(c(
                    1, 0, 0,
                    0, 1, 1
                ),
                dim = c(3L, 2L),
                dimnames = list(
                    q1 = c("A", "B", "C"),
                    wave = c("2014-12-01", "2015-01-01")
                )
                )
            )
            expect_equivalent(
                as.array(kube)[, , "Bird"],
                array(c(
                    0, 0, 0,
                    1, 0, 0
                ),
                dim = c(3L, 2L),
                dimnames = list(
                    q1 = c("A", "B", "C"),
                    wave = c("2014-12-01", "2015-01-01")
                )
                )
            )
            # and now with No Data
            kube@useNA <- "always"
            result <- as.array(kube)[, , "Cat"]
            expect_equivalent(
                result,
                array(c(
                    1, 1, 0, 0, 1, 0,
                    2, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0
                ),
                dim = c(6L, 3L),
                dimnames = list(
                    q1 = c("A", "B", "C", "D", "E", "No Data"),
                    wave = c("2014-12-01", "2015-01-01", "<NA>")
                )
                )
            )
        })

        test_that("Categorical Array cube", {
            kube <- crtabs(~petloc, data = ds)
            expect_equivalent(
                as.array(kube),
                array(c(
                    5, 6,
                    3, 4,
                    3, 6
                ),
                dim = c(2L, 3L),
                dimnames = list(
                    petloc = c("Home", "Work"),
                    petloc = c("Cat", "Dog", "Bird")
                )
                )
            )
            kube@useNA <- "always"
            expect_equivalent(
                as.array(kube),
                array(c(
                    5, 6,
                    3, 4,
                    3, 6,
                    4, 3,
                    5, 1,
                    0, 0
                ),
                dim = c(2L, 6L),
                dimnames = list(
                    petloc = c("Home", "Work"),
                    petloc = c("Cat", "Dog", "Bird", "Skipped", "Not Asked", "No Data")
                )
                )
            )
        })
        test_that("CA x categorical", {
            kube <- crtabs(~ petloc + country, data = ds)
            expect_equivalent(
                as.array(kube)[, , "Belgium"],
                array(c(
                    2, 2,
                    0, 1,
                    0, 0
                ),
                dim = c(2L, 3L),
                dimnames = list(
                    petloc = c("Home", "Work"),
                    petloc = c("Cat", "Dog", "Bird")
                )
                )
            )
        })
    })
})
