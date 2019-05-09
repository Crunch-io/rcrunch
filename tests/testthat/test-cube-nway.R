context("Cubes with >2 dimensions")

with_test_authentication({
    with(test.dataset(newDatasetFromFixture("apidocs")), {
        # Remove this adding of no data chunk once the "default values"
        # ticket https://www.pivotaltracker.com/story/show/164939686 is released.
        # This section ensures that q1 and petloc both have "No Data" categories 
        # even if the server did not make one on creation.
        if (!"No Data" %in% names(categories(ds$q1))) {
            categories(ds$q1) <- c(categories(ds$q1), Category(data = .no.data))
        }
        if (!"No Data" %in% names(categories(ds$petloc))) {
            categories(ds$petloc) <- c(categories(ds$petloc), Category(data = .no.data))
        }
        names(categories(ds$q1)) <- c(LETTERS[1:5], "No Data") ## To distinguish from other vars
        test_that("Cat x datetime x subvar", {
            kube <- crtabs(~q1 + wave + petloc$petloc_home, data = ds)
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
            if (length(result) == 18) {
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
            } else {
                # Remove this branch once the "default values" ticket
                # https://www.pivotaltracker.com/story/show/164939686 is released,
                # because it won't be correct output anymore.
                expect_equivalent(
                    result,
                    array(c(
                        1, 1, 0, 0, 1, 0,
                        2, 0, 0, 0, 0, 0
                    ),
                    dim = c(6L, 2L),
                    dimnames = list(
                        q1 = c("A", "B", "C", "D", "E", "No Data"),
                        wave = c("2014-12-01", "2015-01-01")
                    )
                    )
                )
            }
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
            kube <- crtabs(~petloc + country, data = ds)
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
