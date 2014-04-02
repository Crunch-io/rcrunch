context("Variable catalog")

with(fake.HTTP, {
    variables.catalog.url <- "api/datasets/dataset1/variables.json"
    varblob <- GET(variables.catalog.url)
    
    test_that("VariableCatalog instantiates from Shoji", {
        expect_true(inherits(do.call("VariableCatalog", varblob),
            "VariableCatalog"))
    })
    
    varcat <- do.call("VariableCatalog", varblob)
    varorder <- do.call(VariableGrouping,
        GET("api/datasets/dataset1/hierarchical.json")$groups) ## this seems wrong, shouldn't select "groups" out, should inherit from ShojiObject
    
    test_that("VariableCatalog has the right contents", {
        expect_identical(names(varcat@index), names(varblob$index))
        expect_true(all(grepl("api/datasets/dataset1/variables",
            names(varcat@index))))
        expect_identical(self(varcat), variables.catalog.url)
        expect_identical(ordering(varcat), varorder)
    })
    
    test_that("active/hidden getters", {
        
    })
})