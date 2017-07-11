context("Conditional transform")

with_mock_crunch({
    ds <- loadDataset("test ds")
    test_that("conditionalTransform", {
        expect_error(conditionalTransform("gender", data = ds),
                     'no conditions have been supplied; please supply formulas as conditions.')
        expect_error(conditionalTransform("bar"~"foo", data = ds),
                     'The LHS provided is not a CrunchLogicalExpr: "bar"')
        expect_error(conditionalTransform(gender~"foo", data = ds, type="unknown"),
                     "type must be either ", dQuote("categorical"), " or ",
                     dQuote("text"))
        expect_error(conditionalTransform(gender~"foo", data = ds, type="text",
                                          categories=c("foo", "bar")),
                     "type is not ", dQuote("categorical"), " ignoring ",
                     dQuote("categories"))
        new_var <- conditionalTransform(gender == "Male" ~ textVar, data = ds)
        expect_equal(new_var$values, c(-1, -1, -1, -1, -1, -1, 2, -1, -1, -1,
                                       3, -1, 4, -1, -1, -1, -1, -1, -1, -1, 1,
                                       6, 3, -1, 5))
        expect_equal(new_var$type, "categorical")
        
        new_var <- conditionalTransform(gender == "Male" ~ textVar, data = ds,
                                        categories = c("l", "m", "s", "h", "z", "x"))
        expect_equal(new_var$values, c(-1, -1, -1, -1, -1, -1, 1, -1, -1, -1,
                                       2, -1, 3, -1, -1, -1, -1, -1, -1, -1, 4,
                                       5, 2, -1, 6))
        expect_equal(new_var$type, "categorical")
                
        new_var <- conditionalTransform(gender == "Male" ~ textVar, data = ds,
                                        type = "text")
        expect_equal(new_var$values, c(NA, NA, NA, NA, NA, NA, "l", NA, NA, NA, "m",
                                NA, "s", NA, NA, NA, NA, NA, NA, NA, "h", "z",
                                "m", NA, "x"))
        expect_equal(new_var$type, "text")
        
        new_var <- conditionalTransform(ds$gender == "Male" ~ ds$textVar)
        expect_equal(new_var$values, c(-1, -1, -1, -1, -1, -1, 2, -1, -1, -1,
                                       3, -1, 4, -1, -1, -1, -1, -1, -1, -1, 1,
                                       6, 3, -1, 5))
        expect_equal(new_var$type, "categorical")
        
        new_var <- conditionalTransform(gender == "Male" ~ "guy", data = ds,
                                        type = "text")
        expect_equal(new_var$values, c(NA, NA, NA, NA, NA, NA, "guy", NA, NA, NA, "guy",
                                       NA, "guy", NA, NA, NA, NA, NA, NA, NA, "guy", "guy",
                                       "guy", NA, "guy"))
        expect_equal(new_var$type, "text")
        
        new_var <- conditionalTransform(gender == "Male" ~ 1, data = ds,
                                        type = "numeric")
        expect_equal(new_var$values, c(NA, NA, NA, NA, NA, NA, 1, NA, NA, NA, 1,
                                       NA, 1, NA, NA, NA, NA, NA, NA, NA, 1, 1,
                                       1, NA, 1))
        expect_equal(new_var$type, "numeric")
        
        expect_error(conditionalTransform(gender == "Male" ~ textVar,
                                          data = ds,categories = c("l", "m", 
                                                                   "s", "h", 
                                                                   "z")),
                     "there were categories in the results \\(x\\) that were not specified in categories")
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

        ds$new1 <- conditionalTransform(ndogs < 1 ~ country,
                                    ndogs == 1 ~ q3,
                                    ndogs > 1 ~ ndogs, 
                                    data = ds, else_condition = "other")
        expect_equal(as.vector(ds$new1), factor(c("Jasmine", "other", "2", "3", "Zeus", "2", "2", "3",
                                   "2", "2", "2", "other", "3", "Belgium", "6", "Fluffy",
                                   "other", "Austria", "other", "2")))

        ds$new2 <- conditionalTransform(ndogs < 1 ~ country,
                                    ndogs == 1 ~ q3,
                                    ndogs > 1 ~ ndogs, 
                                    data = ds, type = "text")
        expect_equal(as.vector(ds$new2), c("Jasmine", NA, "2", "3", "Zeus", "2", "2", "3",
                                   "2", "2", "2", NA, "3", "Belgium", "6", "Fluffy",
                                   NA, "Austria", NA, "2"))

        ds$new3 <- conditionalTransform(ndogs < 1 ~ 200,
                                    ndogs == 1 ~ 400,
                                    ndogs > 1 ~ ndogs,
                                    data = ds, type = "numeric")
        expect_equal(as.vector(ds$new3), c(400, NA, 2, 3, 400, 2, 2, 3,
                            2, 2, 2, NA, 3, 200, 6, 400,
                            NA, 200, NA, 2))

        ds$new4 <- conditionalTransform(ndogs < 1 ~ "lonely",
                                    ndogs == 1 ~ q3,
                                    ndogs > 1 ~ ndogs,
                                    data = ds)
        expect_equal(as.vector(ds$new4), factor(c("Jasmine", NA, "2", "3", "Zeus", "2", "2", "3",
                            "2", "2", "2", NA, "3", "lonely", "6", "Fluffy",
                            NA, "lonely", NA, "2")))
    })
})
