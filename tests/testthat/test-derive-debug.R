context("Derived array variables maintain subvar links")

# derived ds instantaitor
new_ds_with_derived_array <- function () {
    ds <- newDatasetFromFixture("apidocs")
    ds$derivedarray <- deriveArray(subvariables = subvariables(ds$petloc),
                                   name="Derived pets")
    return(ds)
}

with_test_authentication({
    ds <- new_ds_with_derived_array()
    
    test_that("Sending a derived array vardef creates a derived array", {
        expect_true(is.derived(ds$derivedarray))
        expect_equivalent(as.vector(ds$derivedarray), as.vector(ds$petloc))
        expect_equivalent(as.vector(ds$derivedarray, mode = "id"),
                          as.vector(ds$petloc, mode = "id"))
    })
    
    test_that("changing a value in the first subvar carries", {
        ds$petloc$petloc_home[ds$petloc$petloc_home == "Dog"] <- "Cat"

        expect_true(is.derived(ds$derivedarray))
        expect_equivalent(as.vector(ds$derivedarray), as.vector(ds$petloc))
        expect_equivalent(as.vector(ds$derivedarray, mode = "id"),
                          as.vector(ds$petloc, mode = "id"))
    })
    
    test_that("changing a value in the second subvar carries", {
        ds$petloc$petloc_work[ds$petloc$petloc_work == "Dog"] <- "Cat"
        
        expect_true(is.derived(ds$derivedarray))
        expect_equivalent(as.vector(ds$derivedarray), as.vector(ds$petloc))
        expect_equivalent(as.vector(ds$derivedarray, mode = "id"),
                          as.vector(ds$petloc, mode = "id"))
    })
    
    # reinstantiate the dataset so prior failures don't cloud current tests
    ds <- new_ds_with_derived_array()
    
    test_that("NAing a value carries", {
        ds$petloc$petloc_home[ds$petloc$petloc_home == "Bird"] <- NA
        
        expect_true(is.derived(ds$derivedarray))
        expect_equivalent(as.vector(ds$derivedarray), as.vector(ds$petloc))
        expect_equivalent(as.vector(ds$derivedarray, mode = "id"),
                          as.vector(ds$petloc, mode = "id"))
    })
    
    # reinstantiate the dataset so prior failures don't cloud current tests
    ds <- new_ds_with_derived_array()
    
    test_that("changing category names in metadata carries", {
        names(categories(ds$petloc)) <- c("Kat", "Dogz", "Bird",
                                          "Skipped", "Not Asked")
        ds <- refresh(ds) # must refresh to update the derived variable's metadata
        
        expect_true(is.derived(ds$derivedarray))
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
    
    # change category ids
    ds$petloc <- changeCategoryID(ds$petloc, 1, 10)
    ds <- refresh(ds) # must refresh to update the derived variable's metadata
    
    test_that("changing cat ids (values+metadata) metadata", {
        expect_true(is.derived(ds$derivedarray))
        expect_equivalent(categories(ds$derivedarray),
                          categories(ds$petloc))
        expect_equivalent(categories(ds$derivedarray$`petloc_work#`),
                          categories(ds$petloc$petloc_work))
        expect_equivalent(categories(ds$derivedarray$`petloc_work#`),
                          categories(ds$petloc$petloc_work))
    })
   
    test_that("changing cat ids (values+metadata) first subvar", {
        # check the first subvar
        expect_equivalent(as.vector(ds$derivedarray$`petloc_home#`),
                          as.vector(ds$petloc$petloc_home))
        expect_equivalent(as.vector(ds$derivedarray$`petloc_home#`, mode = "id"),
                          as.vector(ds$petloc$petloc_home, mode = "id"))
    })
    
    test_that("changing cat ids (values+metadata) second subvar", {        
        # check the second subvar
        expect_equivalent(as.vector(ds$derivedarray$`petloc_work#`),
                          as.vector(ds$petloc$petloc_work))
        expect_equivalent(as.vector(ds$derivedarray$`petloc_work#`, mode = "id"),
                          as.vector(ds$petloc$petloc_work, mode = "id"))
    })
    
    test_that("changing cat ids (values+metadata) whole array", {        
        # check the whole array
        expect_equivalent(as.vector(ds$derivedarray),
                          as.vector(ds$petloc))
        expect_equivalent(as.vector(ds$derivedarray, mode = "id"),
                          as.vector(ds$petloc, mode = "id"))
    })
    
})