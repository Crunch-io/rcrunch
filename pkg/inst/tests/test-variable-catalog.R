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
        expect_identical(active(varcat)@index,
            varcat@index[entities(ordering(varcat))])
        expect_identical(hidden(varcat)@index, list())
        varcat@index[[2]]$discarded <- TRUE
        expect_true(inherits(active(varcat), "VariableCatalog"))
        expect_true(inherits(hidden(varcat), "VariableCatalog"))
        expect_identical(names(active(varcat)@index), 
            c("api/datasets/dataset1/variables/gender.json",
            "api/datasets/dataset1/variables/mymrset.json",
            "api/datasets/dataset1/variables/textVar.json",
            "api/datasets/dataset1/variables/starttime.json"))
        expect_identical(names(hidden(varcat)@index),
            "api/datasets/dataset1/variables/birthyr.json")
        expect_identical(active(hidden(varcat)), hidden(active(varcat)))
    })
    
    test_that("Extract methods", {
        expect_true(inherits(varcat[["api/datasets/dataset1/variables/gender.json"]], "VariableTuple"))
        expect_identical(varcat[["api/datasets/dataset1/variables/gender.json"]]@body,
            varcat@index[["api/datasets/dataset1/variables/gender.json"]])
        expect_identical(varcat[2:3], varcat@index[2:3])
    })
    
    test_that("entity method for tuple", {
        expect_true(is.Categorical(entity(varcat[["api/datasets/dataset1/variables/gender.json"]])))
    })
})