context("Conditional transform")

with_mock_crunch({
    ds <- loadDataset("test ds")
    ds2 <- loadDataset("ECON.sav")

    test_that("conditionalTransform input validation", {
        expect_error(conditionalTransform("gender", data = ds),
                     'no conditions have been supplied; please supply formulas as conditions.')
        expect_error(conditionalTransform("bar"~"foo", data = ds),
                     'The LHS provided is not a CrunchLogicalExpr: "bar"')
        expect_error(conditionalTransform(gender~"foo", data = ds, type="unknown"),
                     paste0("type must be either ", dQuote("categorical"), ", ", dQuote("text"), ", or ", dQuote("numeric")))
        expect_error(conditionalTransform(gender~"foo", data = ds, type="text",
                                          categories=c("foo", "bar")),
                     paste0("type is not ", dQuote("categorical"), " ignoring ",
                     dQuote("categories")))
        # check that we can't reference two different datasets
        expect_error(conditionalTransform(ds$gender == 'Male' ~ "foo",
                                          ds2$gender == 'Male' ~ "foo",
                                          name = "new"),
                     paste0("There is more than one dataset referenced. Please ",
                            "supply only one."))
    })

    test_that("conditionalTransform works with categories", {
        expect_silent(new_var <- conditionalTransform(gender == "Male" ~ textVar, data = ds))
        expect_equal(new_var$values, c(-1, -1, -1, -1, -1, -1, 2, -1, -1, -1,
                                       3, -1, 4, -1, -1, -1, -1, -1, -1, -1, 1,
                                       6, 3, -1, 5))
        expect_equal(new_var$type, "categorical")

        expect_silent(new_var <- conditionalTransform(ds$gender == "Male" ~ ds$textVar))
        expect_equal(new_var$values, c(-1, -1, -1, -1, -1, -1, 2, -1, -1, -1,
                                       3, -1, 4, -1, -1, -1, -1, -1, -1, -1, 1,
                                       6, 3, -1, 5))
        expect_equal(new_var$type, "categorical")

        expect_error(conditionalTransform(gender == "Male" ~ textVar,
                                          data = ds,categories = c("l", "m",
                                                                   "s", "h",
                                                                   "z")),
                     "there were categories in the results \\(x\\) that were not specified in categories")
    })

    test_that("conditionalTransform works when specifying a categories as strings", {
        expect_silent(new_var <- conditionalTransform(gender == "Male" ~ textVar, data = ds,
                                        categories = c("l", "m", "s", "h", "z", "x")))
        expect_equal(new_var$values, c(-1, -1, -1, -1, -1, -1, 1, -1, -1, -1,
                                       2, -1, 3, -1, -1, -1, -1, -1, -1, -1, 4,
                                       5, 2, -1, 6))
        expect_equal(new_var$type, "categorical")
    })

    test_that("conditionalTransform works when specifying a categories object", {
        # use different numeric values and missingnesses to check that the categories object is being sent
        textVarCatslist <- list(
            list(id=1L, name="l", numeric_value=10L, missing=FALSE),
            list(id=2L, name="m", numeric_value=20L, missing=TRUE),
            list(id=3L, name="s", numeric_value=30L, missing=FALSE),
            list(id=4L, name="h", numeric_value=40L, missing=TRUE),
            list(id=5L, name="z", numeric_value=50L, missing=FALSE),
            list(id=6L, name="x", numeric_value=60L, missing=TRUE))
        no_data_cat <- list(id=-1L, name="No Data", numeric_value=NULL, missing=TRUE)
        textVarCats <- Categories(data = textVarCatslist)
        expect_true(is.categories(textVarCats))
        expect_silent(new_var <- conditionalTransform(gender == "Male" ~ textVar, data = ds,
                                                      categories = textVarCats))
        expect_equal(new_var$values, c(-1, -1, -1, -1, -1, -1, 1, -1, -1, -1,
                                       2, -1, 3, -1, -1, -1, -1, -1, -1, -1, 4,
                                       5, 2, -1, 6))
        expect_equal(new_var$type, "categorical")
        expect_json_equivalent(new_var$categories, c(textVarCatslist, list(no_data_cat)))

        # reverse the ids to make sure that the ids are not being over-written
        textVarCatslist <- list(
            list(id=6L, name="l", numeric_value=10L, missing=FALSE),
            list(id=5L, name="m", numeric_value=20L, missing=FALSE),
            list(id=4L, name="s", numeric_value=30L, missing=FALSE),
            list(id=3L, name="h", numeric_value=40L, missing=FALSE),
            list(id=2L, name="z", numeric_value=50L, missing=FALSE),
            list(id=1L, name="x", numeric_value=60L, missing=FALSE))
        textVarCats <- Categories(data = textVarCatslist)
        expect_true(is.categories(textVarCats))
        expect_silent(new_var <- conditionalTransform(gender == "Male" ~ textVar, data = ds,
                                        categories = textVarCats))
        # expect_equal(new_var$values, c(-1, -1, -1, -1, -1, -1, 1, -1, -1, -1,
        #                                2, -1, 3, -1, -1, -1, -1, -1, -1, -1, 4,
        #                                5, 2, -1, 6)) # for standard IDs
        expect_equal(new_var$values, c(-1, -1, -1, -1, -1, -1, 6, -1, -1, -1,
                                       5, -1, 4, -1, -1, -1, -1, -1, -1, -1, 3,
                                       2, 5, -1, 1)) # for reversed IDs
        expect_equal(new_var$type, "categorical")
        expect_json_equivalent(new_var$categories, c(textVarCatslist, list(no_data_cat)))
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
})

with_test_authentication({
    ds <- newDatasetFromFixture("apidocs")
    test_that("conditionalTransform", {
        ds$new0 <- conditionalTransform(ndogs < 1 ~ country,
                                    ndogs == 1 ~ q3,
                                    ndogs > 1 ~ ndogs,
                                    data = ds)
        expect_equal(as.vector(ds$new0), factor(c("Jasmine", NA, "2", "3", "Zeus",
                                              "2", "2", "3", "2", "2", "2", NA,
                                              "3", "Belgium", "6", "Fluffy",
                                              NA, "Austria", NA, "2")))
    })
    test_that("conditionalTransform with else_condition", {
        ds$new1 <- conditionalTransform(ndogs < 1 ~ country,
                                    ndogs == 1 ~ q3,
                                    ndogs > 1 ~ ndogs,
                                    data = ds, else_condition = "other")
        expect_equal(as.vector(ds$new1), factor(c("Jasmine", "other", "2", "3", "Zeus", "2", "2", "3",
                                   "2", "2", "2", "other", "3", "Belgium", "6", "Fluffy",
                                   "other", "Austria", "other", "2")))
    })
    test_that("conditionalTransform with text", {
        ds$new2 <- conditionalTransform(ndogs < 1 ~ country,
                                    ndogs == 1 ~ q3,
                                    ndogs > 1 ~ ndogs,
                                    data = ds, type = "text")
        expect_equal(as.vector(ds$new2), c("Jasmine", NA, "2", "3", "Zeus", "2", "2", "3",
                                   "2", "2", "2", NA, "3", "Belgium", "6", "Fluffy",
                                   NA, "Austria", NA, "2"))
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
        expect_equal(as.vector(ds$new4), factor(c("Jasmine", NA, "2", "3", "Zeus", "2", "2", "3",
                            "2", "2", "2", NA, "3", "lonely", "6", "Fluffy",
                            NA, "lonely", NA, "2")))
    })
    test_that("conditionalTransform with categories", {
        ds$new5 <- conditionalTransform(ndogs < 1 ~ "lonely",
                                        ndogs == 1 ~ q3,
                                        ndogs > 1 ~ ndogs,
                                        data = ds,
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
        expect_equal(as.vector(ds$new6), factor(c("Jasmine", "not applicable", "2", "3", "Zeus", "2", "2", "3",
                                                  "2", "2", "2", "not applicable", "3", "lonely", "6", "Fluffy",
                                                  "not applicable", "lonely", "not applicable", "2")))
        ds$new7 <- conditionalTransform(ndogs < 1 ~ NA,
                                        ndogs == 1 ~ q3,
                                        ndogs > 1 ~ ndogs,
                                        data = ds)
        expect_equal(as.vector(ds$new7), factor(c("Jasmine", NA, "2", "3", "Zeus", "2", "2", "3",
                                                  "2", "2", "2", NA, "3", NA, "6", "Fluffy",
                                                  NA, NA, NA, "2")))
    })
})
