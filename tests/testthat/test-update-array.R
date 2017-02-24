context("Update array variables")

with_test_authentication({
    ds <- mrdf.setup(newDataset(mrdf))
    test_that("Subvariable values before trying to update", {
        expect_equivalent(as.vector(ds$CA$mr_1),
            as.factor(c("1.0", "0.0", "1.0", NA)))
        expect_equivalent(as.vector(ds$CA$mr_2),
            as.factor(c("0.0", "0.0", "1.0", NA)))
        expect_equivalent(as.vector(ds$CA$mr_3),
            as.factor(c("0.0", "0.0", "1.0", NA)))
    })
    test_that("Can update array subvariables", {
        ds$CA[ds$v4 == "B"] <- "1.0"
        expect_equivalent(as.vector(ds$CA$mr_1),
            as.factor(c("1.0", "0.0", "1.0", NA)))
        expect_equivalent(as.vector(ds$CA$mr_2),
            as.factor(c("1.0", "0.0", "1.0", NA)))
        expect_equivalent(as.vector(ds$CA$mr_3),
            as.factor(c("1.0", "0.0", "1.0", NA)))
    })

    ds <- newDatasetFromFixture("apidocs")
    test_that("Can update an individual subvariable conditionally", {
        expect_equivalent(as.vector(ds$allpets$allpets_1, mode="id")[1:5],
            c(1, 9, 1, 1, 9))
        ds$allpets$allpets_1[2] <- 1
        expect_equivalent(as.vector(ds$allpets$allpets_1, mode="id")[1:5],
            c(1, 1, 1, 1, 9))
    })
    test_that("Can update where two subvariables are equal", {
        expect_equal(diag(as.array(crtabs(~ allpets$allpets_1 + allpets$allpets_2,
            data=ds, useNA="always"))),
            c(`not selected`=1, selected=1, `not asked`=3, skipped=1))
        expect_equal(as.array(crtabs(~ allpets$allpets_1,
            data=ds[ds$allpets$allpets_1 == ds$allpets$allpets_2], useNA="always")),
            array(c(1, 1, 3, 1),
                dim=4L,
                dimnames=list(
                    allpets_1=c("not selected", "selected", "not asked", "skipped")
                )))
        expect_equal(as.array(crtabs(~ allpets$allpets_3,
            data=ds[ds$allpets$allpets_1 == ds$allpets$allpets_2], useNA="always")),
            array(c(3, 0, 1, 2),
                dim=4L,
                dimnames=list(
                    allpets_3=c("not selected", "selected", "not asked", "skipped")
                )))
        expect_length(as.vector(ds$q1[ds$allpets$allpets_1 == ds$allpets$allpets_2]),
            6)
        
        ds$allpets$allpets_3[ds$allpets$allpets_1 == ds$allpets$allpets_2] <- "selected"
        expect_equal(as.array(crtabs(~ allpets$allpets_3,
            data=ds[ds$allpets$allpets_1 == ds$allpets$allpets_2], useNA="always")),
            array(c(0, 6, 0, 0),
                dim=4L,
                dimnames=list(
                    allpets_3=c("not selected", "selected", "not asked", "skipped")
                )))
    })
})
