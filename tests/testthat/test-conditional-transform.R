context("Conditional transform")

with_mock_crunch({
    ds <- loadDataset("test ds")
    print(ds)
    test_that("conditionalTransform", {
        expect_error(conditionalTransform(ds$gender),
                     'The case provided is not a formula: ds\\$gender')
        expect_error(conditionalTransform("bar"~"foo"),
                     'The LHS provided is not a CrunchLogicalExpr: "bar"')
        expect_error(conditionalTransform(ds$gender~"foo", type="unknown"),
                     "type must be either ", dQuote("categorical"), " or ", dQuote("text"))
        expect_error(conditionalTransform(ds$gender~"foo", type="text", categories=c("foo", "bar")),
                     "type is not ", dQuote("categorical"), " ignoring ", dQuote("categories"))
        # TODO: Figure out why the mock crunch expression response is not working
        # new_var <- conditionalTransform(ds$gender == "Male" ~ ds$textVar)
    })
})

with_test_authentication({
    ds <- newDatasetFromFixture("apidocs")
    test_that("conditionalTransform", {
        out <- conditionalTransform(ds$ndogs < 1 ~ ds$country,
                                    ds$ndogs == 1 ~ ds$q3,
                                    ds$ndogs > 1 ~ ds$ndogs)
        expect_equal(out, factor(c("Jasmine", NA, "2", "3", "Zeus", "2", "2", "3",
                            "2", "2", "2", NA, "3", "Belgium", "6", "Fluffy",
                            NA, "Austria", NA, "2")))

        out <- conditionalTransform(ds$ndogs < 1 ~ ds$country,
                                    ds$ndogs == 1 ~ ds$q3,
                                    ds$ndogs > 1 ~ ds$ndogs, default_value = "other")
        expect_equal(out, factor(c("Jasmine", "other", "2", "3", "Zeus", "2", "2", "3",
                                   "2", "2", "2", "other", "3", "Belgium", "6", "Fluffy",
                                   "other", "Austria", "other", "2")))

        out <- conditionalTransform(ds$ndogs < 1 ~ ds$country,
                                    ds$ndogs == 1 ~ ds$q3,
                                    ds$ndogs > 1 ~ ds$ndogs, type = "text")
        expect_equal(out, c("Jasmine", NA, "2", "3", "Zeus", "2", "2", "3",
                                   "2", "2", "2", NA, "3", "Belgium", "6", "Fluffy",
                                   NA, "Austria", NA, "2"))

        out <- conditionalTransform(ds$ndogs < 1 ~ 200,
                                    ds$ndogs == 1 ~ 400,
                                    ds$ndogs > 1 ~ ds$ndogs, type = "numeric")
        expect_equal(out, c(400, NA, 2, 3, 400, 2, 2, 3,
                            2, 2, 2, NA, 3, 200, 6, 400,
                            NA, 200, NA, 2))

        out <- conditionalTransform(ds$ndogs < 1 ~ "lonely",
                                    ds$ndogs == 1 ~ ds$q3,
                                    ds$ndogs > 1 ~ ds$ndogs)
        expect_equal(out, factor(c("Jasmine", NA, "2", "3", "Zeus", "2", "2", "3",
                            "2", "2", "2", NA, "3", "lonely", "6", "Fluffy",
                            NA, "lonely", NA, "2")))
    })
})
