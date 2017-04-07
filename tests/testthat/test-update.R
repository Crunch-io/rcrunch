context("Update a dataset")

with_mock_HTTP({
    ds <- loadDataset("test ds")
    test_that("Updating values makes a POST request to the table endpoint", {
        ## TODO: assert the payload shape. This is mainly about exercising code
        ## in the unit tests. We test the behavior in integration tests
        expect_POST(ds$gender <- "Male",
            "api/datasets/1/table/")
        expect_POST(ds$birthyr <- 1970,
            "api/datasets/1/table/")
        expect_POST(ds$mymrset <- rep(c(1, 2), 10),
            "api/datasets/1/table/")
        expect_POST(ds$gender[ds$birthyr > 2020] <- "Male",
            "api/datasets/1/table/")
        expect_POST(ds$birthyr[ds$birthyr > 2020] <- 1970,
            "api/datasets/1/table/")
        expect_POST(ds$mymrset[ds$birthyr > 2020] <- 1,
            "api/datasets/1/table/")
    })

    test_that("Validation on categorical update", {
        expect_error(ds$gender[is.na(ds$birthyr)] <- as.factor(c("Male", "Other", "Prefer not to say", "Female")),
            "Input values Other and Prefer not to say are not present in the category names of variable")
        expect_error(ds$gender[is.na(ds$birthyr)] <- 3,
            "Input value 3 is not present in the category ids of variable")
        expect_error(ds$gender[1:2] <- c(NA, -1),
            "Cannot have both NA and -1 when specifying category ids")
        expect_error(ds$gender[1:2] <- FALSE,
            "Cannot update CategoricalVariable with type logical")
        expect_error(ds$gender <- FALSE,
            "Cannot update CategoricalVariable with type logical")
    })

    test_that("Validation on filtered updating", {
        expect_error(ds$gender[ds$gender == "Male"] <- ds$gender[ds$birthyr == 1999],
            "Cannot update a variable with an expression that has a different filter")
        expect_error(ds$birthyr[ds$gender == "Male"] <- ds$birthyr[ds$gender == "Female"],
            "Cannot update a variable with an expression that has a different filter")
        expect_error(ds$birthyr[ds$gender == "Male"] <- ds$birthyr[ds$gender == "Female"] + 2,
            "Cannot update a variable with an expression that has a different filter")

        skip("TODO: distinguish this kind of attempted value updating from metadata updates")
        expect_error(ds$gender <- ds$gender[ds$birthyr == 1999],
            "Cannot update a variable with an expression that has a different filter")
        ## Now add filter to dataset
        ds2 <- ds[ds$gender == "Male",]
        expect_error(ds2$gender <- ds$gender[ds$birthyr == 1999],
            "Cannot update a variable with an expression that has a different filter")
        expect_error(ds2$birthyr <- ds$birthyr[ds$gender == "Female"],
            "Cannot update a variable with an expression that has a different filter")
        expect_error(ds2$birthyr <- ds$birthyr[ds$gender == "Female"] + 2,
            "Cannot update a variable with an expression that has a different filter")

        ## TODO: further filter on filtered dataset/variable
    })

    test_that("Can't update variable with logical expression", {
        expect_error(ds$gender[ds$gender == "Male"] <- is.na(ds$gender[ds$gender == "Male"]),
            "Cannot update CategoricalVariable with type CrunchLogicalExpr")
        skip("TODO: distinguish this kind of attempted value updating from metadata updates")
        expect_error(ds$gender <- is.na(ds$gender),
            "Cannot update CategoricalVariable with type CrunchLogicalExpr")
    })

    test_that("Trying to update with the wrong data type fails", {
        expect_error(ds$birthyr[is.na(ds$gender)] <- letters[3:7],
            "Cannot update NumericVariable with type character")
    })
})

with_test_authentication({
    ds <- newDataset(df)
    test_that("Can update numeric variable with values", {
        ds$v3 <- 9:28
        test <- as.vector(ds$v3) - df$v3
        expect_true(all(test == 1))
    })

    ds$v3 <- 1
    test_that("Value recycling on insert is consistent with R", {
        expect_true(all(as.vector(ds$v3) == 1))
    })

    ds$v3[1:10] <- 2
    test_that("Update numeric with R numeric filter and values", {
        expect_equivalent(mean(ds$v3), 1.5)
    })
    ds$v3[ds$v3 == 1] <- 3
    test_that("Update numeric with LogicalExpression filter", {
        expect_equivalent(mean(ds$v3), 2.5)
    })
    ds[ds$v3 == 2, "v3"] <- 4
    test_that("Update with LogicalExpression within dataset", {
        expect_equivalent(mean(ds$v3), 3.5)
    })
    ds$v3 <- c(rep(5, 10), rep(7, 10))
    test_that("Just update the values", {
        expect_equivalent(mean(ds$v3), 6)
    })

    test_that("Can update numeric variable with expresssion", {
        ds$v3 <- ds$v3 + 2
        expect_equivalent(as.vector(ds$v3), c(rep(7, 10), rep(9, 10)))
    })

    test_that("Can filter on is.na", {
        ds$v3[is.na(ds$v2)] <- 0
        expect_equivalent(as.vector(ds$v3),
            c(rep(7, 10), rep(9, 5), rep(0, 5)))
    })

    test_that("Can update text", {
        ds$v2[is.na(ds$v1)] <- "z"
        expect_identical(as.vector(ds$v2)[1:8],
            c(rep("z", 5), "f", "g", "h"))
        ds[ds$v2 %in% "z", "v2"] <- "y"
        expect_identical(as.vector(ds$v2)[1:8],
            c(rep("y", 5), "f", "g", "h"))
    })

    test_that("Can update datetime", {
        newvals <- as.Date(0:12, origin="1985-10-26")
        ds$v5[ds$v5 >= as.Date("1955-11-12")] <- newvals
        expect_identical(max(ds$v5), as.Date("1985-11-07"))
    })

    date.before <- rep(c("2014-04-15", "2014-08-15"), 2)
    date.after <- c("2014-04-15", "2014-09-15", "2014-04-15",
        "2014-09-15")
    date.df <- data.frame(wave=as.Date(date.before))
    with(test.dataset(date.df, "date.ds"), {
        test_that("Another datetime update", {
            expect_identical(as.vector(date.ds$wave),
                as.Date(date.before))
            date.ds$wave[date.ds$wave == as.Date("2014-08-15")] <- as.Date("2014-09-15")
            expect_identical(as.vector(date.ds$wave),
                as.Date(date.after))
        })
    })

    ## Categorical
    ds$v4[is.na(ds$v2)] <- "B"
    test_that("Can update categorical variables with character", {
        expect_equivalent(table(ds$v4)["B"], c(B=13L))
    })
    ds$v4[is.na(ds$v2)] <- factor("C")
    test_that("Can update categorical with factor", {
        expect_equivalent(table(ds$v4)["C"], c(C=12L))
    })
    ds$v4[is.na(ds$v2)] <- c(2,1,2,1,2)
    test_that("Can update categorical with numeric (ids)", {
        expect_equivalent(table(ds$v4), table(df$v4))
    })

    ## Logical -> Categorical
    test_that("Category names for logical-as-categorical", {
        expect_identical(names(categories(ds$v6)),
            c("True", "False", "No Data"))
    })
    test_that("Can edit values of a logical-as-categorical with logical", {
        ds$v6[c(2, 5)] <- FALSE
        expect_equal(as.vector(ds$v6[1:5], "id"), c(1, 2, 1, 1, 2))
        ds$v6[c(6, 9)] <- c(FALSE, NA)
        expect_equal(as.vector(ds$v6[1:10], "id"),
            c(1, 2, 1, 1, 2, 2, 1, 1, -1, 1))
    })
    test_that("Can't edit a regular categorical with logical", {
        expect_error(ds$v4[5] <- TRUE,
            "Cannot update CategoricalVariable with type logical")
    })
})
