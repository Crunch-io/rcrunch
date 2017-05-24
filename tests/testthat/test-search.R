context("Searching for datasets and variables")

with_mock_HTTP({
    results <- searchDatasets("gender")

    test_that("Search queries", {
        expect_is(results, "SearchResults")
    })

    test_that("Datasets in search results", {
        expect_is(datasets(results), "DatasetCatalog")
        expect_identical(names(datasets(results)),
            c("ACS 2012", "Economist/YouGov survey, 13 Jul 2013"))
    })

    test_that("Variables in search results", {
        expect_is(variables(results), "VariableCatalog")
        expect_identical(names(variables(results)),
            c("gender", "pp_gender", "Weight (gender)", "Sex", "Gender x Marstat"))
    })
})
