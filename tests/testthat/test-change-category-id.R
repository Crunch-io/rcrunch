context("changeCategoryID")

with_mock_crunch({
    ds <- loadDataset("test ds")

    test_that("changeCategoryID input validation", {
        expect_error(
            ds$birthyr <- changeCategoryID(ds$birthyr, 1, 6),
            "The variable Birth Year doesn't have categories."
        )
        expect_error(
            ds$gender <- changeCategoryID(ds$gender, 1, -1),
            "Id -1 is already a category, please provide a new category id."
        )
        expect_error(
            ds$gender <- changeCategoryID(ds$gender, "not a numeric", 1),
            "from should be a single numeric"
        )
        expect_error(
            ds$gender <- changeCategoryID(ds$gender, "not a numeric", c(1, -1)),
            "from should be a single numeric"
        )
        expect_error(
            ds$gender <- changeCategoryID(ds$gender, c(1, -1), 99),
            "from should be a single numeric"
        )
        expect_error(
            ds$gender <- changeCategoryID(ds$gender, 1, "not a numeric"),
            "to should be a single numeric"
        )
        expect_error(
            ds$gender <- changeCategoryID(ds$gender, 99, c(1, -1)),
            "to should be a single numeric"
        )
        expect_error(
            ds$gender <- changeCategoryID(ds$gender, 8, 9),
            "No category with id 8"
        )
    })

    test_that("changeCategoryID's first request is made", {
        expect_PATCH(
            changeCategoryID(ds$gender, 2, 6),
            "https://app.crunch.io/api/datasets/1/variables/gender/",
            '{"categories":[{"id":1,"missing":false,"name":"Male",',
            '"numeric_value":1},{"id":2,"missing":false,"name":',
            '"__TO_DELETE__","numeric_value":2},{"id":-1,',
            '"missing":true,"name":"No Data","numeric_value":null}]}'
        )
    })
})

with_test_authentication({
    ds <- newDataset(data.frame(
        samevalue=df$v4[1:4],
        diffvalue=df$v4[1:4],
        excluded=df$v4[1:4],
        nothree=1:4
    ))
    values(categories(ds$diffvalue)) <- c(NA, 20, NA)
    orig_vector <- as.vector(ds$samevalue)

    test_that("Setup for changing category IDs", {
        # they're all the same, so just check `samevalue`
        expect_identical(
            names(categories(ds$samevalue)),
            c("B", "C", "No Data")
        )
        expect_equal(
            ids(categories(ds$samevalue)),
            c(1, 2, -1)
        )
        expect_equal(as.vector(ds$samevalue, mode = "id"), c(1, 2, 1, 2))

        expect_equal(
            values(categories(ds$diffvalue)),
            c(NA, 20, NA)
        )
    })

    test_that("Can changeCategoryID, and numeric_value changes when same as id", {
        expect_silent(ds$samevalue <- changeCategoryID(ds$samevalue, 2, 6))
        expect_identical(
            names(categories(ds$samevalue)),
            c("B", "C", "No Data")
        )
        expect_equal(
            ids(categories(ds$samevalue)),
            c(1, 6, -1)
        )
        expect_equal(as.vector(ds$samevalue), orig_vector)
        expect_equal(as.vector(ds$samevalue, mode = "id"), c(1, 6, 1, 6))
        # Note that the numeric_value is changed too because before it was
        # the same as the id
        expect_equal(as.vector(ds$samevalue, mode = "numeric"), c(1, 6, 1, 6))
    })

    test_that("Can changeCategoryID without changing values when value != id", {
        expect_silent(ds$diffvalue <- changeCategoryID(ds$diffvalue, 2, 6))
        expect_identical(
            names(categories(ds$diffvalue)),
            c("B", "C", "No Data")
        )
        expect_equal(
            ids(categories(ds$diffvalue)),
            c(1, 6, -1)
        )
        expect_equal(
            values(categories(ds$diffvalue)),
            c(NA, 20, NA)
        )
        expect_equal(as.vector(ds$diffvalue), orig_vector)
        expect_equal(as.vector(ds$diffvalue, mode = "id"), c(1, 6, 1, 6))
        expect_equal(as.vector(ds$diffvalue, mode = "numeric"), c(NA, 20, NA, 20))

        # also try with an NA, make sure the NA is retained
        expect_silent(ds$diffvalue <- changeCategoryID(ds$diffvalue, 1, 10))
        expect_identical(
            names(categories(ds$diffvalue)),
            c("B", "C", "No Data")
        )
        expect_equal(
            ids(categories(ds$diffvalue)),
            c(10, 6, -1)
        )
        expect_equal(
            values(categories(ds$diffvalue)),
            c(NA, 20, NA)
        )
        expect_equal(as.vector(ds$diffvalue, mode = "id"), c(10, 6, 10, 6))
        expect_equal(as.vector(ds$diffvalue, mode = "numeric"), c(NA, 20, NA, 20))
    })

    # set exclusion
    exclusion(ds) <- ds$nothree == 3
    test_that("Can changeCategoryID with an exclusion", {
        expect_equal(
            as.vector(ds$excluded, mode = "id"),
            c(1, 2, 2)
        )

        expect_warning(ds$excluded <- changeCategoryID(ds$excluded, 1, 4),
            paste0(
                "Temporarily disabling the exclusion while ",
                "changing category IDs. See ",
                "`?changeCategoryID` for more information."
            ),
            fixed = TRUE
        )
        expect_identical(
            names(categories(ds$excluded)),
            c("B", "C", "No Data")
        )
        expect_equal(
            ids(categories(ds$excluded)),
            c(4, 2, -1)
        )
        expect_equal(
            as.vector(ds$excluded, mode = "id"),
            c(4, 2, 2)
        )
        # and finally, the exclusion is back (still there)
        expect_equivalent(exclusion(ds), ds$nothree == 3)
    })

    test_that("Can changeCategoryID for array variables", {
        ds_apidocs <- newDatasetFromFixture("apidocs")
        # categorical array variables
        expect_identical(
            names(categories(ds_apidocs$petloc)),
            c("Cat", "Dog", "Bird", "Skipped", "Not Asked", "No Data")
        )
        expect_equal(
            ids(categories(ds_apidocs$petloc)),
            c(1, 2, 3, 8, 9, -1)
        )
        expect_equal(dim(as.vector(ds_apidocs$petloc)), c(20, 2))
        orig_vector <- as.vector(ds_apidocs$petloc)
        expect_equal(
            as.vector(ds_apidocs$petloc[1:4], mode = "id"),
            data.frame(
                petloc_home = c(8, 2, 9, 9),
                petloc_work = c(9, 3, 3, 2)
            )
        )

        ds_apidocs$petloc <- changeCategoryID(ds_apidocs$petloc, 2, 6)
        expect_identical(
            names(categories(ds_apidocs$petloc)),
            c("Cat", "Dog", "Bird", "Skipped", "Not Asked", "No Data")
        )
        expect_equal(
            ids(categories(ds_apidocs$petloc)),
            c(1, 6, 3, 8, 9, -1)
        )
        expect_equal(dim(as.vector(ds_apidocs$petloc)), c(20, 2))
        expect_equal(as.vector(ds_apidocs$petloc), orig_vector)
        expect_equal(
            as.vector(ds_apidocs$petloc[1:4], mode = "id"),
            data.frame(
                petloc_home = c(8, 6, 9, 9),
                petloc_work = c(9, 3, 3, 6)
            )
        )
    })
})
