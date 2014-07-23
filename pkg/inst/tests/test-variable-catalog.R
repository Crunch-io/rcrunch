context("Variable catalog")

with(fake.HTTP, {
    variables.catalog.url <- "api/datasets/dataset1/variables.json"
    varblob <- GET(variables.catalog.url)
    
    test_that("VariableCatalog instantiates from Shoji", {
        expect_true(inherits(VariableCatalog(varblob),
            "VariableCatalog"))
    })
    
    varcat <- VariableCatalog(varblob)
    varorder <- do.call(VariableOrder,
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
        varcat@index[[5]]$discarded <- TRUE
        expect_true(inherits(active(varcat), "VariableCatalog"))
        expect_true(inherits(hidden(varcat), "VariableCatalog"))
        expect_identical(names(active(varcat)@index), 
            c("api/datasets/dataset1/variables/gender.json",
            "api/datasets/dataset1/variables/mymrset.json",
            "api/datasets/dataset1/variables/textVar.json",
            "api/datasets/dataset1/variables/starttime.json"))
        expect_identical(length(active(varcat)), 4L)
        expect_identical(names(hidden(varcat)@index),
            "api/datasets/dataset1/variables/birthyr.json")
        expect_identical(length(hidden(varcat)), 1L)
        expect_identical(length(varcat), 8L)
        expect_identical(active(hidden(varcat)), hidden(active(varcat)))
    })
    
    test_that("Extract methods", {
        expect_true(inherits(varcat[["api/datasets/dataset1/variables/gender.json"]], "VariableTuple"))
        expect_identical(varcat[["api/datasets/dataset1/variables/gender.json"]]@body,
            varcat@index[["api/datasets/dataset1/variables/gender.json"]])
        expect_identical(varcat[2:3]@index, varcat@index[2:3])
        expect_error(varcat[[999]], "subscript out of bounds")
        skip({
            expect_error(varcat[["asdf"]], "subscript out of bounds")
            expect_error(varcat[999:1000], "subscript out of bounds")
        }, "Not implemented")
    })
    
    test_that("entity method for tuple", {
        expect_true(is.Categorical(entity(varcat[["api/datasets/dataset1/variables/gender.json"]])))
    })
    
    test_that("name and alias getters", {
        expect_identical(names(varcat)[1:3], c("Gender", "Second", "First"))
        expect_identical(aliases(varcat)[1:2], c("gender", "subvar1"))
    })
})

if (!run.only.local.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            ds <- .setup
            test_that("can set names and aliases", {
                n <- names(df)
                expect_identical(names(variables(ds)), n)
                expect_identical(aliases(variables(ds)), n)
                names(variables(ds))[2:3] <- c("two", "three")
                n2 <- n
                n2[2:3] <- c("two", "three")
                expect_identical(names(variables(ds)), n2)
                expect_identical(names(variables(refresh(ds))), n2)
                n3 <- n
                n3[c(2,4)] <- c("due", "quattro")
                aliases(variables(ds))[c(2,4)] <- c("due", "quattro")
                expect_identical(aliases(variables(ds)), n3)
                expect_identical(aliases(variables(refresh(ds))), n3)
            })
        })
    })
}