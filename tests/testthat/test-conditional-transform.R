context("Conditional transform")

with_mock_crunch({
    ds <- loadDataset("test ds")
    test_that("conditionalTransform", {
        expect_error(conditionalTransform("gender", data = ds),
                     'The case provided is not a formula: ', dQuote("gender"))
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
        expect_equal(new_var, factor(c(NA, NA, NA, NA, NA, NA, "l", NA, NA, NA,
                                       "m", NA, "s", NA, NA, NA, NA, NA, NA,
                                       NA, "h", "z", "m", NA, "x")))
        new_var <- conditionalTransform(gender == "Male" ~ textVar, data = ds,
                                        type = "text")
        expect_equal(new_var, c(NA, NA, NA, NA, NA, NA, "l", NA, NA, NA, "m",
                                NA, "s", NA, NA, NA, NA, NA, NA, NA, "h", "z",
                                "m", NA, "x"))
        new_var <- conditionalTransform(ds$gender == "Male" ~ ds$textVar)
        expect_equal(new_var, factor(c(NA, NA, NA, NA, NA, NA, "l", NA, NA, NA,
                                       "m", NA, "s", NA, NA, NA, NA, NA, NA,
                                       NA, "h", "z", "m", NA, "x")))
    })
})

with_test_authentication({
    ds <- newDatasetFromFixture("apidocs")
    test_that("conditionalTransform", {
        out <- conditionalTransform(ndogs < 1 ~ country,
                                    ndogs == 1 ~ q3,
                                    ndogs > 1 ~ ndogs, 
                                    data = ds)
        expect_equal(out, factor(c("Jasmine", NA, "2", "3", "Zeus", "2", "2", "3",
                            "2", "2", "2", NA, "3", "Belgium", "6", "Fluffy",
                            NA, "Austria", NA, "2")))

        out <- conditionalTransform(ndogs < 1 ~ country,
                                    ndogs == 1 ~ q3,
                                    ndogs > 1 ~ ndogs, 
                                    data = ds, else_condition = "other")
        expect_equal(out, factor(c("Jasmine", "other", "2", "3", "Zeus", "2", "2", "3",
                                   "2", "2", "2", "other", "3", "Belgium", "6", "Fluffy",
                                   "other", "Austria", "other", "2")))

        out <- conditionalTransform(ndogs < 1 ~ country,
                                    ndogs == 1 ~ q3,
                                    ndogs > 1 ~ ndogs, 
                                    data = ds, type = "text")
        expect_equal(out, c("Jasmine", NA, "2", "3", "Zeus", "2", "2", "3",
                                   "2", "2", "2", NA, "3", "Belgium", "6", "Fluffy",
                                   NA, "Austria", NA, "2"))

        out <- conditionalTransform(ndogs < 1 ~ 200,
                                    ndogs == 1 ~ 400,
                                    ndogs > 1 ~ ndogs,
                                    data = ds, type = "numeric")
        expect_equal(out, c(400, NA, 2, 3, 400, 2, 2, 3,
                            2, 2, 2, NA, 3, 200, 6, 400,
                            NA, 200, NA, 2))

        out <- conditionalTransform(ndogs < 1 ~ "lonely",
                                    ndogs == 1 ~ q3,
                                    ndogs > 1 ~ ndogs,
                                    data = ds)
        expect_equal(out, factor(c("Jasmine", NA, "2", "3", "Zeus", "2", "2", "3",
                            "2", "2", "2", NA, "3", "lonely", "6", "Fluffy",
                            NA, "lonely", NA, "2")))
    })
})
