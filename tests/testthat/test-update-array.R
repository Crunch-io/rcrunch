context("Update array variables")

with_test_authentication({
    ds <- mrdf.setup(newDataset(mrdf))
    test_that("Subvariable values before trying to update", {
        expect_equivalent(
            as.vector(ds$CA$mr_1),
            as.factor(c("1.0", "0.0", "1.0", NA))
        )
        expect_equivalent(
            as.vector(ds$CA$mr_2),
            as.factor(c("0.0", "0.0", "1.0", NA))
        )
        expect_equivalent(
            as.vector(ds$CA$mr_3),
            as.factor(c("0.0", "0.0", "1.0", NA))
        )
    })
    test_that("Can update array subvariables", {
        ds$CA[ds$v4 == "B"] <- "1.0"
        expect_equivalent(
            as.vector(ds$CA$mr_1),
            as.factor(c("1.0", "0.0", "1.0", NA))
        )
        expect_equivalent(
            as.vector(ds$CA$mr_2),
            as.factor(c("1.0", "0.0", "1.0", NA))
        )
        expect_equivalent(
            as.vector(ds$CA$mr_3),
            as.factor(c("1.0", "0.0", "1.0", NA))
        )
    })

    ds <- newDatasetFromFixture("apidocs")
    test_that("Can update an individual subvariable conditionally", {
        expect_equivalent(
            as.vector(ds$allpets$allpets_1, mode = "id")[1:5],
            c(1, 9, 1, 1, 9)
        )
        ds$allpets$allpets_1[2] <- 1
        expect_equivalent(
            as.vector(ds$allpets$allpets_1, mode = "id")[1:5],
            c(1, 1, 1, 1, 9)
        )
    })
    test_that("Can update where two subvariables are equal", {
        # Replace this `expect_true(isTRUE(all.equal(new)) || isTRUE(all.equal(old)))`
        # construction with `expect_equal(new)` once the "default values"
        # ticket https://www.pivotaltracker.com/story/show/164939686 is released.
        expect_true(
            isTRUE(all.equal(
                diag(as.array(crtabs(~allpets$allpets_1 + allpets$allpets_2,
                    data = ds, useNA = "always"
                ))),
                c(`not selected` = 1, selected = 1, `not asked` = 3, skipped = 1, `No Data` = 0)
            ))
            # Legacy output, if "No Data" categories are not automatically added:
            || isTRUE(all.equal(
                diag(as.array(crtabs(~allpets$allpets_1 + allpets$allpets_2,
                    data = ds, useNA = "always"
                ))),
                c(`not selected` = 1, selected = 1, `not asked` = 3, skipped = 1)
            ))
        )
        # Replace this `expect_true(isTRUE(all.equal(new)) || isTRUE(all.equal(old)))`
        # construction with `expect_equal(new)` once the "default values"
        # ticket https://www.pivotaltracker.com/story/show/164939686 is released.
        expect_true(
            isTRUE(all.equal(
                as.array(crtabs(~allpets$allpets_1,
                    data = ds[ds$allpets$allpets_1 == ds$allpets$allpets_2], useNA = "always"
                )),
                array(c(1, 1, 3, 1, 0),
                    dim = 5L,
                    dimnames = list(
                        allpets_1 = c("not selected", "selected", "not asked", "skipped", "No Data")
                    )
                )
            ))
            # Legacy output, if "No Data" categories are not automatically added:
            || isTRUE(all.equal(
                as.array(crtabs(~allpets$allpets_1,
                    data = ds[ds$allpets$allpets_1 == ds$allpets$allpets_2], useNA = "always"
                )),
                array(c(1, 1, 3, 1),
                    dim = 4L,
                    dimnames = list(
                        allpets_1 = c("not selected", "selected", "not asked", "skipped")
                    )
                )
            ))
        )
        # Replace this `expect_true(isTRUE(all.equal(new)) || isTRUE(all.equal(old)))`
        # construction with `expect_equal(new)` once the "default values"
        # ticket https://www.pivotaltracker.com/story/show/164939686 is released.
        expect_true(
            isTRUE(all.equal(
                as.array(crtabs(~allpets$allpets_3,
                    data = ds[ds$allpets$allpets_1 == ds$allpets$allpets_2], useNA = "always"
                )),
                array(c(3, 0, 1, 2, 0),
                    dim = 5L,
                    dimnames = list(
                        allpets_3 = c("not selected", "selected", "not asked", "skipped", "No Data")
                    )
                )
            ))
            # Legacy output, if "No Data" categories are not automatically added:
            || isTRUE(all.equal(
                as.array(crtabs(~allpets$allpets_3,
                    data = ds[ds$allpets$allpets_1 == ds$allpets$allpets_2], useNA = "always"
                )),
                array(c(3, 0, 1, 2),
                    dim = 4L,
                    dimnames = list(
                        allpets_3 = c("not selected", "selected", "not asked", "skipped")
                    )
                )
            ))
        )
        expect_length(
            as.vector(ds$q1[ds$allpets$allpets_1 == ds$allpets$allpets_2]),
            6
        )

        ds$allpets$allpets_3[ds$allpets$allpets_1 == ds$allpets$allpets_2] <- "selected"
        # Replace this `expect_true(isTRUE(all.equal(new)) || isTRUE(all.equal(old)))`
        # construction with `expect_equal(new)` once the "default values"
        # ticket https://www.pivotaltracker.com/story/show/164939686 is released.
        expect_true(
            isTRUE(all.equal(
                as.array(crtabs(~allpets$allpets_3,
                    data = ds[ds$allpets$allpets_1 == ds$allpets$allpets_2], useNA = "always"
                )),
                array(c(0, 6, 0, 0, 0),
                    dim = 5L,
                    dimnames = list(
                        allpets_3 = c("not selected", "selected", "not asked", "skipped", "No Data")
                    )
                )
            ))
            # Legacy output, if "No Data" categories are not automatically added:
            || isTRUE(all.equal(
                as.array(crtabs(~allpets$allpets_3,
                    data = ds[ds$allpets$allpets_1 == ds$allpets$allpets_2], useNA = "always"
                )),
                array(c(0, 6, 0, 0),
                    dim = 4L,
                    dimnames = list(
                        allpets_3 = c("not selected", "selected", "not asked", "skipped")
                    )
                )
            ))
        )
    })
})
