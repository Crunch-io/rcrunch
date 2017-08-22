context("Formulas")


with_mock_crunch({
    ds <- loadDataset("test ds")
    
    test_that("parseTerms", {
        rhs_data <- parseTerms(~gender+birthyr, data=ds)
        expect_equivalent(rhs_data, list(ds$gender, ds$birthyr))
        lhs_data <- parseTerms(birthyr~gender, data=ds, side = "LHS")
        expect_equivalent(lhs_data, list(ds$birthyr))    
        lhs_data <- parseTerms(~gender, data=ds, side = "LHS")
        expect_equivalent(lhs_data, list())   
        expect_error(parseTerms(~gender, data=ds, side = "not a side"),
                     "unknown side specification for parsing formulae.")
        expect_error(parseTerms(~1, data=ds, side = "LHS"),
                     "Must supply one or more variables")
        expect_error(parseTerms(.~gender, data=ds, side = "LHS"),
                     paste0("Crunch formulae do not support ", dQuote("."), " in formula"))
    })
    
    test_that("formulaToQuery can accept both data and calling env. references", {
        ftq_with_data <- formulaToQuery(~gender+birthyr, data=ds)
        expect_is(ftq_with_data, "list")
        ftq_without_data <- formulaToQuery(~ds$gender+ds$birthyr)
        expect_is(ftq_without_data, "list")
        expect_equal(ftq_with_data, ftq_without_data)
    })
})