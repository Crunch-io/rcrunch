context("Conditional transform")

with_mock_crunch({
    ds <- loadDataset("test ds")
})

with_test_authentication({
    ds <- newDatasetFromFixture("apidocs")
    test_that("conditionalTransform", {
        out <- conditionalTransform(cases = list(ds$ndogs < 1, ds$ndogs == 1, ds$ndogs > 1),
                                    values=list(ds$country, ds$q3, ds$ndogs))
        expect_equal(out, c("Jasmine", NA, "2", "3", "Zeus", "2", "2", "3",
                            "2", "2", "2", NA, "3", "Belgium", "6", "Fluffy",
                            NA, "Austria", NA, "2"))
    })
})
