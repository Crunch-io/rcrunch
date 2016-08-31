context("Deep copies of variables")

with_test_authentication({
    ds <- newDatasetFromFixture("apidocs")
    test_that("Can deep copy categorical", {
        ds$q1a <- copy(ds$q1, deep=TRUE)
        expect_identical(as.vector(ds$q1), as.vector(ds$q1a))
    })
    test_that("Can deep copy numeric", {
        ds$ndogsa <- copy(ds$ndogs, deep=TRUE)
        expect_identical(as.vector(ds$ndogs), as.vector(ds$ndogsa))
    })
    test_that("Can deep copy datetime", {
        ds$wavea <- copy(ds$wave, deep=TRUE)
        expect_identical(as.vector(ds$wave), as.vector(ds$wavea))
    })
    test_that("Can deep copy multiple response", {
        expect_error(copy(ds$allpets, deep=TRUE),
            "Deep copying of multiple-response variables is not implemented.")
        skip("Needs https://www.pivotaltracker.com/story/show/86161264")
        ds$allpetsa <- copy(ds$allpets, deep=TRUE)
        for (i in 1:2) {
            ## Whole thing isn't identical because the aliases are different
            expect_identical(as.vector(ds$allpets[[i]]),
                as.vector(ds$allpetsa[[i]]))
        }
        expect_equivalent(as.array(crtabs(~ allpets, data=ds)),
            as.array(crtabs(~ allpetsa, data=ds)))
    })
    test_that("Can deep copy categorical array", {
        ds$petloca <- copy(ds$petloc, deep=TRUE)
        for (i in 1:2) {
            ## Whole thing isn't identical because the aliases are different
            expect_identical(as.vector(ds$petloc[[i]]),
                as.vector(ds$petloca[[i]]))
        }
        expect_equivalent(as.array(crtabs(~ petloc, data=ds)),
            as.array(crtabs(~ petloca, data=ds)))
    })

    part2 <- newDatasetFromFixture("apidocs")
    test_that("Deep copies don't get data when appending", {
        out <- appendDataset(ds, part2)
        ## Counts should be double in the original than in the copy
        expect_equivalent(as.array(crtabs(~ q1, data=out)),
            2 * as.array(crtabs(~ q1a, data=out)))
        expect_equivalent(as.array(crtabs(~ petloc, data=out)),
            2 * as.array(crtabs(~ petloca, data=out)))
    })
})
