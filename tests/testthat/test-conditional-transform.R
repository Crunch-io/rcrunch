context("Conditional transform")

with_mock_crunch({
    ds <- loadDataset("test ds")
    ds2 <- loadDataset("ECON.sav")

    test_that("conditionalTransform input validation", {
        expect_error(conditionalTransform("gender", data = ds),
                     'no conditions have been supplied; please supply formulas as conditions.')
        expect_error(conditionalTransform("bar" ~ "foo", data = ds),
                     'The left-hand side provided is not a CrunchLogicalExpr: "bar"')

        expect_error(conditionalTransform(gender ~ "foo", data = ds, type="unknown"),
                     paste0("type must be either ", dQuote("categorical"), ", ",
                            dQuote("text"), ", or ", dQuote("numeric")))

        expect_warning(conditionalTransform(gender == 'Male' ~ "foo", data = ds, type="text",
                                          categories=c("foo", "bar")),
                     paste0("type is not ", dQuote("categorical"), " ignoring ",
                     dQuote("categories")))
        # check that we can't reference two different datasets
        expect_error(conditionalTransform(ds$gender == 'Male' ~ "foo",
                                          ds2$gender == 'Male' ~ "foo",
                                          name = "new"),
                     paste0("There is more than one dataset referenced. Please ",
                            "supply only one."))

        # updated to use a categorical variable as the source
        expect_error(conditionalTransform(gender == "Male" ~ textVar,
                                          data = ds,
                                          type="categorical",
                                          categories = c("l", "m", "s", "h", "z")),
                     "there were categories in the results \\(x\\) that were not specified in categories")
    })

    test_that("conditionalTransform works with categories", {
        expect_silent(new_var <- conditionalTransform(gender == "Male" ~ textVar,
                                                      data = ds,
                                                      type = "categorical"))
        expect_equal(new_var$values, c(-1, -1, -1, -1, -1, -1, 2, -1, -1, -1,
                                       3, -1, 4, -1, -1, -1, -1, -1, -1, -1, 1,
                                       6, 3, -1, 5))
        expect_equal(new_var$type, "categorical")

        expect_silent(new_var <- conditionalTransform(ds$gender == "Male" ~ ds$textVar,
                                                      type = "categorical"))
        expect_equal(new_var$values, c(-1, -1, -1, -1, -1, -1, 2, -1, -1, -1,
                                       3, -1, 4, -1, -1, -1, -1, -1, -1, -1, 1,
                                       6, 3, -1, 5))
        expect_equal(new_var$type, "categorical")
    })

    test_that("conditionalTransform works when specifying a categories as strings", {
        expect_silent(new_var <-
                          conditionalTransform(gender == "Male" ~ textVar,
                                               data = ds,
                                               type = "categorical",
                                               categories = c("l", "m", "s",
                                                              "h", "z", "x")))
        expect_equal(new_var$values, c(-1, -1, -1, -1, -1, -1, 1, -1, -1, -1,
                                       2, -1, 3, -1, -1, -1, -1, -1, -1, -1, 4,
                                       5, 2, -1, 6))
        expect_equal(new_var$type, "categorical")
    })

    test_that("conditionalTransform works when specifying a categories object", {
        # use different numeric values and missingnesses to check that the
        # categories object is being sent
        textVarCats <- Categories(
            list(id=1L, name="l", numeric_value=10L, missing=FALSE),
            list(id=2L, name="m", numeric_value=20L, missing=TRUE),
            list(id=3L, name="s", numeric_value=30L, missing=FALSE),
            list(id=4L, name="h", numeric_value=40L, missing=TRUE),
            list(id=5L, name="z", numeric_value=50L, missing=FALSE),
            list(id=6L, name="x", numeric_value=60L, missing=TRUE))
        no_data_cat <- Categories(list(id=-1L, name="No Data", numeric_value=NULL, missing=TRUE))
        textVarCats <- Categories(data = textVarCats)
        expect_true(is.categories(textVarCats))
        expect_silent(new_var <- conditionalTransform(gender == "Male" ~ textVar,
                                                      data = ds, type = "categorical",
                                                      categories = textVarCats))
        expect_equal(new_var$values, c(-1, -1, -1, -1, -1, -1, 1, -1, -1, -1,
                                       2, -1, 3, -1, -1, -1, -1, -1, -1, -1, 4,
                                       5, 2, -1, 6))
        expect_equal(new_var$type, "categorical")
        expect_json_equivalent(new_var$categories, c(textVarCats, no_data_cat))

        # reverse the ids to make sure that the ids are not being over-written
        textVarCats <- Categories(
            list(id=6L, name="l", numeric_value=10L, missing=FALSE),
            list(id=5L, name="m", numeric_value=20L, missing=FALSE),
            list(id=4L, name="s", numeric_value=30L, missing=FALSE),
            list(id=3L, name="h", numeric_value=40L, missing=FALSE),
            list(id=2L, name="z", numeric_value=50L, missing=FALSE),
            list(id=1L, name="x", numeric_value=60L, missing=FALSE))
        textVarCats <- Categories(data = textVarCats)
        expect_true(is.categories(textVarCats))
        expect_silent(new_var <- conditionalTransform(gender == "Male" ~ textVar,
                                                      data = ds,
                                                      type = "categorical",
                                                      categories = textVarCats))
        # expect_equal(new_var$values, c(-1, -1, -1, -1, -1, -1, 1, -1, -1, -1,
        #                                2, -1, 3, -1, -1, -1, -1, -1, -1, -1, 4,
        #                                5, 2, -1, 6)) # for standard IDs
        expect_equal(new_var$values, c(-1, -1, -1, -1, -1, -1, 6, -1, -1, -1,
                                       5, -1, 4, -1, -1, -1, -1, -1, -1, -1, 3,
                                       2, 5, -1, 1)) # for reversed IDs
        expect_equal(new_var$type, "categorical")
        expect_json_equivalent(new_var$categories, c(textVarCats, no_data_cat))
    })

    test_that("conditionalTransform works when specifying a categories erroneously", {
        textVarCats <- Categories(
            list(id=1L, name="l", numeric_value=10L, missing=FALSE),
            list(id=2L, name="m", numeric_value=20L, missing=TRUE),
            list(id=3L, name="s", numeric_value=30L, missing=FALSE),
            list(id=4L, name="h", numeric_value=40L, missing=TRUE),
            list(id=5L, name="z", numeric_value=50L, missing=FALSE),
            list(id=6L, name="x", numeric_value=60L, missing=TRUE))
        no_data_cat <- Categories(list(id=-1L, name="No Data", numeric_value=NULL, missing=TRUE))
        textVarCats <- Categories(data = textVarCats)
        expect_true(is.categories(textVarCats))
        expect_warning(new_var <- conditionalTransform(gender == "Male" ~ textVar,
                                                      data = ds,
                                                      categories = textVarCats),
                       paste0("type is not ", dQuote("categorical"),
                              " ignoring ", dQuote("categories")))
        expect_equal(new_var$values, c(NA, NA, NA, NA, NA, NA, "l", NA, NA, NA, "m",
                                       NA, "s", NA, NA, NA, NA, NA, NA, NA, "h", "z",
                                       "m", NA, "x"))
        expect_equal(new_var$type, "text")
    })

    test_that("conditionalTransform works with other output types (text and numeric)", {
        expect_silent(new_var <- conditionalTransform(gender == "Male" ~ textVar, data = ds,
                                        type = "text"))
        expect_equal(new_var$values, c(NA, NA, NA, NA, NA, NA, "l", NA, NA, NA, "m",
                                NA, "s", NA, NA, NA, NA, NA, NA, NA, "h", "z",
                                "m", NA, "x"))
        expect_equal(new_var$type, "text")

        expect_silent(new_var <- conditionalTransform(gender == "Male" ~ "guy", data = ds,
                                        type = "text"))
        expect_equal(new_var$values, c(NA, NA, NA, NA, NA, NA, "guy", NA, NA, NA, "guy",
                                       NA, "guy", NA, NA, NA, NA, NA, NA, NA, "guy", "guy",
                                       "guy", NA, "guy"))
        expect_equal(new_var$type, "text")

        expect_silent(new_var <- conditionalTransform(gender == "Male" ~ 1, data = ds,
                                        type = "numeric"))
        expect_equal(new_var$values, c(NA, NA, NA, NA, NA, NA, 1, NA, NA, NA, 1,
                                       NA, 1, NA, NA, NA, NA, NA, NA, NA, 1, 1,
                                       1, NA, 1))
        expect_equal(new_var$type, "numeric")
    })

    test_that("makeConditionalValues", {
        # need to develop mocks for other conditions and sources to test various
        # permutations.
    })

    #####################
    ### check collation
    #####################
    values_to_fill <- list(c("A", "A"), c("B", "B"),
                           c("C","C"), c("D", "D"))
    case_indices <- list(c(1, 3), c(2, 4), c(5, 7), c(6, 8))
    else_condition <- NA
    n_rows <- 8

    test_that("collateValues works with all characters", {
        expect_equal(collateValues(values_to_fill, case_indices, else_condition,
                                   n_rows),
                     c("A", "B", "A", "B", "C", "D", "C", "D"))
    })

    test_that("collateValues works with all NAs", {
        values_to_fill <- list(c("A", "A"), c("B", "B"),
                               c("C","C"), c("D"))
        case_indices <- list(c(1, 3), c(2, 4), c(5, 7), c(8))
        expect_equal(collateValues(values_to_fill, case_indices, else_condition,
                                   n_rows),
                     c("A", "B", "A", "B", "C", NA, "C", "D"))

        values_to_fill <- list(c("A", "A"), c("B", "B"),
                               c("C",NA), c("D"))
        expect_equal(collateValues(values_to_fill, case_indices, else_condition,
                                   n_rows),
                     c("A", "B", "A", "B", "C", NA, NA, "D"))
    })

    test_that("collateValues works with factors", {
        values_to_fill <- list(factor(c("A", "A")), factor(c("B", "B")),
                               factor(c("C","C")), factor(c("D", "D")))
        expect_equal(collateValues(values_to_fill, case_indices, else_condition,
                                   n_rows),
                     c("A", "B", "A", "B", "C", "D", "C", "D"))

        values_to_fill[[3]] <- NULL
        case_indices[[3]] <- NULL
        expect_equal(collateValues(values_to_fill, case_indices, else_condition,
                                   n_rows),
                     c("A", "B", "A", "B", NA, "D", NA, "D"))
    })

    test_that("collateValues works with numerics", {
        values_to_fill <- list(c(10, 10), c(20, 20),
                               c(30,30), c(40, 40))
        expect_equal(collateValues(values_to_fill, case_indices, else_condition,
                                   n_rows),
                     c(10, 20, 10, 20, 30, 40, 30, 40))

        values_to_fill[[3]] <- NULL
        case_indices[[3]] <- NULL
        expect_equal(collateValues(values_to_fill, case_indices, else_condition,
                                   n_rows),
                     c(10, 20, 10, 20, NA, 40, NA, 40))
    })

    test_that("collateValues works with character+numeric", {
        values_to_fill <- list(c(10, 10), c("B", "B"),
                               c(30,30), c("D", "D"))
        expect_equal(collateValues(values_to_fill, case_indices, else_condition,
                                   n_rows),
                     c("10", "B", "10", "B", "30", "D", "30", "D"))

        values_to_fill[[3]] <- NULL
        case_indices[[3]] <- NULL
        expect_equal(collateValues(values_to_fill, case_indices, else_condition,
                                   n_rows),
                     c("10", "B", "10", "B", NA, "D", NA, "D"))
    })

    test_that("collateValues works with character+numeric+factor", {
        values_to_fill <- list(c(10, 10), factor(c("B", "B")),
                               c(30,30), c("D", "D"))
        expect_equal(collateValues(values_to_fill, case_indices, else_condition,
                                   n_rows),
                     c("10", "B", "10", "B", "30", "D", "30", "D"))

        values_to_fill[[3]] <- NULL
        case_indices[[3]] <- NULL
        expect_equal(collateValues(values_to_fill, case_indices, else_condition,
                                   n_rows),
                     c("10", "B", "10", "B", NA, "D", NA, "D"))
    })
})

with_test_authentication({
    ds <- newDatasetFromFixture("apidocs")
    test_that("conditionalTransform", {
        ds$new0 <- conditionalTransform(ndogs < 1 ~ country,
                                    ndogs == 1 ~ q3,
                                    ndogs > 1 ~ ndogs,
                                    data = ds)
        expect_equal(as.vector(ds$new0), c("Jasmine", NA, "2", "3", "Zeus",
                                              "2", "2", "3", "2", "2", "2", NA,
                                              "3", "Belgium", "6", "Fluffy",
                                              NA, "Austria", NA, "2"))
    })
    test_that("conditionalTransform with else_condition", {
        ds$new1 <- conditionalTransform(ndogs < 1 ~ country,
                                    ndogs == 1 ~ q3,
                                    ndogs > 1 ~ ndogs,
                                    data = ds, else_condition = "other")
        expect_equal(as.vector(ds$new1), c("Jasmine", "other", "2", "3", "Zeus", "2", "2", "3",
                                   "2", "2", "2", "other", "3", "Belgium", "6", "Fluffy",
                                   "other", "Austria", "other", "2"))
    })
    test_that("conditionalTransform with text", {
        ds$new2 <- conditionalTransform(ndogs < 1 ~ country,
                                    ndogs == 1 ~ q3,
                                    ndogs > 1 ~ ndogs,
                                    data = ds, type = "categorical")
        expect_equal(as.vector(ds$new2), factor(c("Jasmine", NA, "2", "3", "Zeus", "2", "2", "3",
                                   "2", "2", "2", NA, "3", "Belgium", "6", "Fluffy",
                                   NA, "Austria", NA, "2")))
    })
    test_that("conditionalTransform with numeric", {
        ds$new3 <- conditionalTransform(ndogs < 1 ~ 200,
                                    ndogs == 1 ~ 400,
                                    ndogs > 1 ~ ndogs,
                                    data = ds, type = "numeric")
        expect_equal(as.vector(ds$new3), c(400, NA, 2, 3, 400, 2, 2, 3,
                            2, 2, 2, NA, 3, 200, 6, 400,
                            NA, 200, NA, 2))
    })
    test_that("conditionalTransform with a sole string as source", {
        ds$new4 <- conditionalTransform(ndogs < 1 ~ "lonely",
                                    ndogs == 1 ~ q3,
                                    ndogs > 1 ~ ndogs,
                                    data = ds)
        expect_equal(as.vector(ds$new4), c("Jasmine", NA, "2", "3", "Zeus", "2", "2", "3",
                            "2", "2", "2", NA, "3", "lonely", "6", "Fluffy",
                            NA, "lonely", NA, "2"))
    })
    test_that("conditionalTransform with categories", {
        ds$new5 <- conditionalTransform(ndogs < 1 ~ "lonely",
                                        ndogs == 1 ~ q3,
                                        ndogs > 1 ~ ndogs,
                                        data = ds,
                                        type = "categorical",
                                        categories = c("lonely", "Zeus",
                                                       "Jasmine", "Fluffy",
                                                       "2", "3", "6"))
        expect_equal(as.vector(ds$new5), factor(c("Jasmine", NA, "2", "3", "Zeus", "2", "2", "3",
                                                  "2", "2", "2", NA, "3", "lonely", "6", "Fluffy",
                                                  NA, "lonely", NA, "2"),
                     levels = c("lonely", "Zeus", "Jasmine", "Fluffy", "2", "3", "6")))
    })
    test_that("conditionalTransform with NAs", {
        ds$new6 <- conditionalTransform(ndogs < 1 ~ "lonely",
                                        ndogs == 1 ~ q3,
                                        ndogs > 1 ~ ndogs,
                                        is.na(ndogs) ~ "not applicable",
                                        data = ds)
        expect_equal(as.vector(ds$new6), c("Jasmine", "not applicable", "2", "3", "Zeus", "2", "2", "3",
                                                  "2", "2", "2", "not applicable", "3", "lonely", "6", "Fluffy",
                                                  "not applicable", "lonely", "not applicable", "2"))
        ds$new7 <- conditionalTransform(ndogs < 1 ~ NA,
                                        ndogs == 1 ~ q3,
                                        ndogs > 1 ~ ndogs,
                                        data = ds)
        expect_equal(as.vector(ds$new7), c("Jasmine", NA, "2", "3", "Zeus", "2", "2", "3",
                                                  "2", "2", "2", NA, "3", NA, "6", "Fluffy",
                                                  NA, NA, NA, "2"))
    })

    test_that("conditionalTransform with an exclusion set with a text varaible", {
        exclusion(ds) <- ds$ndogs > 2
        ds$new8 <- conditionalTransform(ndogs < 1 ~ 0,
                                        ndogs == 1 ~ 4,
                                        ndogs > 1 ~ ndogs,
                                        is.na(ndogs) ~ 5,
                                        data = ds)
        expect_equal(as.vector(ds$new8), c(4, 5, 2, 4, 2, 2,
                                           2, 2, 2, 5, 0, 4,
                                           5, 0, 5, 2))
        # and after the exclusion is removed, we get NAs.
        exclusion(ds) <- NULL
        expect_equal(as.vector(ds$new8), c(4, 5, 2, NA, 4, 2, 2, NA,
                                           2, 2, 2, 5, NA, 0, NA, 4,
                                           5, 0, 5, 2))
    })

    test_that("conditionalTransform with an exclusion set with a text varaible", {
        exclusion(ds) <- ds$ndogs > 2
        ds$new9 <- conditionalTransform(ndogs < 1 ~ "lonely",
                                        ndogs == 1 ~ q3,
                                        ndogs > 1 ~ ndogs,
                                        is.na(ndogs) ~ "not applicable",
                                        type = "categorical",
                                        data = ds)
        expect_equal(as.vector(ds$new9), as.factor(c("Jasmine", "not applicable", "2", "Zeus", "2", "2",
                                           "2", "2", "2", "not applicable", "lonely", "Fluffy",
                                           "not applicable", "lonely", "not applicable", "2")))
        # and after the exclusion is removed, we get NAs.
        exclusion(ds) <- NULL
        expect_equal(as.vector(ds$new9), as.factor(c("Jasmine", "not applicable", "2", NA, "Zeus", "2", "2", NA,
                                           "2", "2", "2", "not applicable", NA, "lonely", NA, "Fluffy",
                                           "not applicable", "lonely", "not applicable", "2")))
    })

    test_that("conditionalTransform with an exclusion set with a text varaible", {
        exclusion(ds) <- ds$ndogs > 2
        ds$new10 <- conditionalTransform(ndogs < 1 ~ "lonely",
                                        ndogs == 1 ~ q3,
                                        ndogs > 1 ~ ndogs,
                                        is.na(ndogs) ~ "not applicable",
                                        data = ds)
        expect_equal(as.vector(ds$new10), c("Jasmine", "not applicable", "2", "Zeus", "2", "2",
                                           "2", "2", "2", "not applicable", "lonely", "Fluffy",
                                           "not applicable", "lonely", "not applicable", "2"))
        # and after the exclusion is removed, we get NAs.
        exclusion(ds) <- NULL
        expect_equal(as.vector(ds$new10), c("Jasmine", "not applicable", "2", NA, "Zeus", "2", "2", NA,
                                           "2", "2", "2", "not applicable", NA, "lonely", NA, "Fluffy",
                                           "not applicable", "lonely", "not applicable", "2"))
    })
})
