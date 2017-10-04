context("Derived array variables maintain subvar links")

with_test_authentication({
    ds <- newDatasetFromFixture("apidocs")
    ds$derivedarray <- deriveArray(subvariables = subvariables(ds$petloc),
                                   name="Derived pets")
    
    test_that("Sending a derived array vardef creates a derived array", {
        expect_true(is.derived(ds$derivedarray))
        expect_equivalent(as.vector(ds$derivedarray), as.vector(ds$petloc))
        expect_equivalent(as.vector(ds$derivedarray, mode = "id"),
                          as.vector(ds$petloc, mode = "id"))
    })
    
    test_that("changing a value carries", {
        ds$petloc$petloc_home[ds$petloc$petloc_home == "Dog"] <- "Cat"

        expect_equivalent(as.vector(ds$derivedarray), as.vector(ds$petloc))
        expect_equivalent(as.vector(ds$derivedarray, mode = "id"),
                          as.vector(ds$petloc, mode = "id"))
    })
    
    test_that("NAing a value carries", {
        ds$petloc$petloc_home[ds$petloc$petloc_home == "Bird"] <- NA

        expect_equivalent(as.vector(ds$derivedarray), as.vector(ds$petloc))
        expect_equivalent(as.vector(ds$derivedarray, mode = "id"),
                          as.vector(ds$petloc, mode = "id"))
    })
    
    test_that("changing category names in metadata carries", {
        names(categories(ds$petloc)) <- c("Kat", "Dogz", "Bird",
                                                      "Skipped", "Not Asked",
                                                      "No Data")
        
        expect_equivalent(categories(ds$derivedarray),
                          categories(ds$petloc))
        expect_equivalent(categories(ds$derivedarray$`petloc_work#`),
                          categories(ds$petloc$petloc_work))
        
        # checking the petloc_work subvar since if the above tests failed,
        # we know that petloc_home is broken
        expect_equivalent(as.vector(ds$derivedarray$`petloc_work#`),
                          as.vector(ds$petloc$petloc_work))
        expect_equivalent(as.vector(ds$derivedarray$`petloc_work#`, mode = "id"),
                          as.vector(ds$petloc$petloc_work, mode = "id"))
    })
    
    test_that("changing category ids (values+metadata) carries", {
        ds$petloc <- changeCategoryID(ds$petloc, 1, 10)
        expect_equivalent(categories(ds$derivedarray),
                          categories(ds$petloc))
        expect_equivalent(categories(ds$derivedarray$`petloc_work#`),
                          categories(ds$petloc$petloc_work))
        
        expect_equivalent(as.vector(ds$derivedarray$`petloc_work#`),
                          as.vector(ds$petloc$petloc_work))
        expect_equivalent(as.vector(ds$derivedarray$`petloc_work#`, mode = "id"),
                          as.vector(ds$petloc$petloc_work, mode = "id"))
    })
    
})