context("Formulas")


with_mock_crunch({
    ds <- loadDataset("test ds")
    test_that("formulaToQuery can accept both data and calling env. references", {
        ftq_with_data <- formulaToQuery(~gender+birthyr, data=ds)
        expect_is(ftq_with_data, "list")
        ftq_without_data <- formulaToQuery(~ds$gender+ds$birthyr)
        expect_is(ftq_without_data, "list")
        expect_equal(ftq_with_data, ftq_without_data)
    })
})