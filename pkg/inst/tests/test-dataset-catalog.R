context("Dataset catalog")

with(fake.HTTP, {
    dataset.catalog.url <- "api/datasets.json"
    blob <- GET(dataset.catalog.url)
    
    test_that("DatasetCatalog instantiates from Shoji", {
        expect_true(inherits(do.call("DatasetCatalog", blob),
            "DatasetCatalog"))
    })
    
    datcat <- do.call("DatasetCatalog", blob)
    
    test_that("DatasetCatalog has the right contents", {
        expect_identical(names(datcat@index),
            names(blob$index)[c(2,3,1)]) ## sorting
        expect_true(all(grepl("api/dataset",
            names(datcat@index))))
        expect_identical(self(datcat), dataset.catalog.url)
    })
    
    test_that("active/archived getters", {
        ## NOTE: deferring the "shared" collection
        expect_true(inherits(active(datcat), "DatasetCatalog"))
        expect_true(inherits(archived(datcat), "DatasetCatalog"))
        expect_identical(active(datcat)@index,
            datcat@index)
        expect_equivalent(archived(datcat)@index, list())
        datcat@index[[1]]$archived <- TRUE
        expect_true(inherits(active(datcat), "DatasetCatalog"))
        expect_true(inherits(archived(datcat), "DatasetCatalog"))
        expect_identical(names(active(datcat)@index), 
            c("api/datasets/dataset3.json", "api/datasets/dataset1.json"))
        expect_identical(length(active(datcat)), 2L)
        expect_identical(names(archived(datcat)@index),
            "api/datasets/dataset2.json")
        expect_identical(length(archived(datcat)), 1L)
        expect_identical(length(datcat), 3L)
        expect_identical(active(archived(datcat)), archived(active(datcat)))
    })
    
    test_that("Extract methods", {
        expect_true(inherits(datcat[["api/datasets/dataset1.json"]], "DatasetTuple"))
        expect_identical(datcat[["api/datasets/dataset1.json"]]@body,
            datcat@index[["api/datasets/dataset1.json"]])
        expect_identical(datcat[2:3]@index, datcat@index[2:3])
    })
    
    test_that("entity method for tuple", {
        expect_true(is.dataset(entity(datcat[["api/datasets/dataset1.json"]])))
    })
})