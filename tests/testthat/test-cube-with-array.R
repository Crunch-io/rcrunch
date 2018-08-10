context("Cubes with categorical array and multiple response")

with_test_authentication({
    cubemrdf <- mrdf
    cubemrdf$v5 <- as.factor(c("A", "A", "B", "B"))
    mrds <- mrdf.setup(newDataset(cubemrdf), selections = "1.0")

    test_that("univariate multiple response cube", {
        kube <- crtabs(~MR, data = mrds)
        expect_is(kube, "CrunchCube")
        expect_equivalent(
            as.array(kube),
            array(c(2, 1, 1),
                dim = c(3L),
                dimnames = list(MR = c("mr_1", "mr_2", "mr_3"))
            )
        )
    })

    test_that("bivariate cube with MR", {
        kube <- crtabs(~MR + v4, data = mrds)
        expect_is(kube, "CrunchCube")
        expect_equivalent(
            as.array(kube),
            array(c(2, 1, 1, 0, 0, 0),
                dim = c(3L, 2L),
                dimnames = list(
                    MR = c("mr_1", "mr_2", "mr_3"),
                    v4 = c("B", "C")
                )
            )
        )

        kube <- crtabs(~v4 + MR, data = mrds)
        expect_is(kube, "CrunchCube")
        expect_equivalent(
            as.array(kube),
            array(c(2, 0, 1, 0, 1, 0),
                dim = c(2L, 3L),
                dimnames = list(
                    v4 = c("B", "C"),
                    MR = c("mr_1", "mr_2", "mr_3")
                )
            )
        )

        kube <- crtabs(~v4 + MR, data = mrds, useNA = "ifany")
        expect_is(kube, "CrunchCube")
        expect_equivalent(
            as.array(kube),
            array(c(2, 0, 1, 0, 1, 0),
                dim = c(2L, 3L),
                dimnames = list(
                    v4 = c("B", "C"),
                    MR = c("mr_1", "mr_2", "mr_3")
                )
            )
        )

        kube@useNA <- "always"
        expect_equivalent(
            as.array(kube),
            array(c(
                2, 0, 0,
                1, 0, 0,
                1, 0, 0
            ),
            dim = c(3L, 3L),
            dimnames = list(
                v4 = c("B", "C", "No Data"),
                MR = c("mr_1", "mr_2", "mr_3")
            )
            )
        )
    })

    c1 <- crtabs(~MR, data = mrds)
    test_that("prop.table on univariate MR without NAs", {
        expect_equivalent(
            prop.table(c1),
            array(c(2 / 3, 1 / 3, 1 / 3),
                dim = c(3L),
                dimnames = list(MR = c("mr_1", "mr_2", "mr_3"))
            )
        )
    })
    test_that("prop.table on univariate MR, useNA=always", {
        c2 <- c1
        c2@useNA <- "always"
        expect_equivalent(
            prop.table(c2),
            array(c(.5, .25, .25),
                dim = c(3L),
                dimnames = list(MR = c("mr_1", "mr_2", "mr_3"))
            )
        )
    })

    c1 <- crtabs(~v5 + MR, data = mrds)
    #    MR
    # v5  mr_1 mr_2 mr_3
    #   A    1    0    0
    #   B    1    1    1
    test_that("prop.table on bivariate with MR, no NAs", {
        expect_equivalent(
            as.array(c1),
            array(c(1, 1, 0, 1, 0, 1),
                dim = c(2L, 3L),
                dimnames = list(
                    v5 = c("A", "B"),
                    MR = c("mr_1", "mr_2", "mr_3")
                )
            )
        )
        expect_equivalent(
            margin.table(c1),
            array(c(3, 3, 3),
                dim = c(3L),
                dimnames = list(MR = c("mr_1", "mr_2", "mr_3"))
            )
        )
        expect_equivalent(
            prop.table(c1),
            array(c(1, 1, 0, 1, 0, 1) / 3,
                dim = c(2L, 3L),
                dimnames = list(
                    v5 = c("A", "B"),
                    MR = c("mr_1", "mr_2", "mr_3")
                )
            )
        )
        expect_equivalent(
            margin.table(c1, 1),
            array(c(2, 1, 2, 1, 2, 1),
                dim = c(2L, 3L),
                dimnames = list(
                    v5 = c("A", "B"),
                    MR = c("mr_1", "mr_2", "mr_3")
                )
            )
        )
        expect_equivalent(
            prop.table(c1, margin = 1),
            array(c(.5, 1, 0, 1, 0, 1),
                dim = c(2L, 3L),
                dimnames = list(
                    v5 = c("A", "B"),
                    MR = c("mr_1", "mr_2", "mr_3")
                )
            )
        )
        expect_equivalent(
            margin.table(c1, margin = 2),
            as.array(c(2, 1, 1))
        )
        expect_equivalent(
            prop.table(c1, margin = 2),
            array(c(.5, .5, 0, 1, 0, 1),
                dim = c(2L, 3L),
                dimnames = list(
                    v5 = c("A", "B"),
                    MR = c("mr_1", "mr_2", "mr_3")
                )
            )
        )
    })
    c2 <- c1
    c2@useNA <- "ifany"
    #    MR
    # v5  mr_1 mr_2 mr_3 <NA>
    #   A    1    0    0    0
    #   B    1    1    1    1
    test_that("prop.table on bivariate with MR, margin=NULL, useNA=ifany", {
        expect_equivalent(
            as.array(c2),
            array(c(1, 1, 0, 1, 0, 1),
                dim = c(2L, 3L),
                dimnames = list(
                    v5 = c("A", "B"),
                    MR = c("mr_1", "mr_2", "mr_3")
                )
            )
        )

        ## Sweep the whole table
        expect_equivalent(
            margin.table(c2),
            array(c(4, 4, 4),
                dim = c(3L),
                dimnames = list(MR = c("mr_1", "mr_2", "mr_3"))
            )
        )
        expect_equivalent(
            prop.table(c2),
            array(c(1, 1, 0, 1, 0, 1) / 4,
                dim = c(2L, 3L),
                dimnames = list(
                    v5 = c("A", "B"),
                    MR = c("mr_1", "mr_2", "mr_3")
                )
            )
        )
    })
    test_that("prop.table on bivariate with MR, margin=1, useNA=ifany", {
        expect_equivalent(
            margin.table(c2, 1),
            array(c(2, 2, 2, 2, 2, 2),
                dim = c(2, 3L),
                dimnames = list(
                    v5 = c("A", "B"),
                    MR = c("mr_1", "mr_2", "mr_3")
                )
            )
        )
        expect_equivalent(
            prop.table(c2, margin = 1),
            array(c(.5, .5, 0, .5, 0, .5),
                dim = c(2L, 3L),
                dimnames = list(
                    v5 = c("A", "B"),
                    MR = c("mr_1", "mr_2", "mr_3")
                )
            )
        )
    })
    test_that("prop.table on bivariate with MR, margin=2, useNA=ifany", {
        expect_equivalent(
            margin.table(c2, 2),
            array(c(2, 1, 1),
                dim = c(3L),
                dimnames = list(MR = c("mr_1", "mr_2", "mr_3"))
            )
        )
        expect_equivalent(
            prop.table(c2, margin = 2),
            array(c(.5, .5, 0, 1, 0, 1),
                dim = c(2L, 3L),
                dimnames = list(
                    v5 = c("A", "B"),
                    MR = c("mr_1", "mr_2", "mr_3")
                )
            )
        )
    })

    cube.as.CA <- array(c(1, 2, 2, 2, 1, 1, 1, 1, 1),
        dim = c(3L, 3L),
        dimnames = list(
            CA = c("mr_1", "mr_2", "mr_3"),
            CA = c("0.0", "1.0", "<NA>")
        )
    )
    test_that("Cube of MR as_array", {
        kube <- crtabs(~as_array(MR), data = mrds, useNA = "ifany")
        expect_is(kube, "CrunchCube")
        expect_equivalent(as.array(kube), cube.as.CA)
    })
    mrds$MR <- undichotomize(mrds$MR)
    alias(mrds$MR) <- "CA"
    name(mrds$CA) <- "Cat array"
    test_that("'univariate' categorical array cube", {
        kube <- crtabs(~CA, data = mrds, useNA = "ifany")
        expect_is(kube, "CrunchCube")
        expect_equivalent(as.array(kube), cube.as.CA)
    })

    test_that("accessing array subvariables", {
        kube <- crtabs(~CA$mr_1 + CA$mr_2, data = mrds, useNA = "ifany")
        expect_equivalent(
            as.array(kube),
            array(c(1, 1, 0, 0, 1, 0, 0, 0, 1),
                dim = c(3L, 3L),
                dimnames = list(
                    mr_1 = c("0.0", "1.0", "No Data"),
                    mr_2 = c("0.0", "1.0", "No Data")
                )
            )
        )
    })
})
