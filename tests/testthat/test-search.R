with_mock_crunch({
    results <- searchDatasets("gender")

    test_that("Search queries", {
        expect_is(results, "SearchResults")
    })

    test_that("Datasets in search results", {
        expect_is(datasets(results), "DatasetCatalog")
        expect_identical(
            names(datasets(results)),
            c("ACS 2012", "Economist/YouGov survey, 13 Jul 2013")
        )
    })

    test_that("Variables in search results", {
        expect_is(variables(results), "VariableCatalog")
        expect_identical(
            names(variables(results)),
            c("gender", "pp_gender", "Weight (gender)", "Sex", "Gender x Marstat")
        )
    })

    test_that("Search input validation", {
        expect_error(
            searchDatasets(variables(results)),
            "Search query must be a string, not VariableCatalog"
        )
        expect_error(
            searchDatasets(c("one", "two")),
            "Search query must be a single string, not a length-2 character vector"
        )
    })

    test_that("f argument is JSONified", {
        expect_GET(
            searchDatasets("one", f = list(a = 2)),
            "https://app.crunch.io/api/search/?q=one&grouping=datasets&f=%7B%22a%22%3A2%7D"
            )
    })
})
