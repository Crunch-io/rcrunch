context("Versions")

with(fake.HTTP, {
    ds <- loadDataset("test ds")
    test_that("Version catalog exists", {
        expect_true(inherits(versions(ds), "VersionCatalog"))
    })
    
    test_that("Version catalog attributes (and sorting)", {
        expect_identical(length(versions(ds)), 2L)
        expect_identical(names(versions(ds)), 
            c("another version", "initial load"))
        expect_identical(descriptions(versions(ds)), 
            c("another version", "initial load"))
        expect_identical(strftime(timestamps(versions(ds)), "%d %b %Y"), 
            c("15 Feb 2015", "12 Feb 2015"))
    })
    
    test_that("Version catalog print method", {
        expect_identical(showVersionCatalog(versions(ds),
                            from=strptime("2015-02-17", "%Y-%m-%d")),
            data.frame(Name=c("another version", "initial load"),
                Timestamp=c("1 day ago", "4 days ago"),
                stringsAsFactors=FALSE))
    })
    
    test_that("saveVersion makes the right request", {
        expect_error(saveVersion(ds, "Today"), 
            'POST /api/datasets/dataset1/savepoints.json \\{"description":"Today"\\}')
    })
    
    test_that("restoreVersion makes the right request", {
        expect_error(restoreVersion(ds, "initial load"),
            'POST /api/datasets/dataset1/savepoints/v2/revert/')
        expect_error(restoreVersion(ds, 2),
            'POST /api/datasets/dataset1/savepoints/v2/revert/')
        expect_error(restoreVersion(ds, "not a version"), 
            paste0(dQuote("not a version"), 
            " does not match any available versions"))
    })
})


if (run.integration.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            test_that("Dataset imported correctly", {
                validImport(ds)
            })
            test_that("There is an initial version", {
                expect_identical(length(versions(ds)), 1L)
                expect_identical(names(versions(ds)), "initial import")
            })

            ## Make changes:
            # 1. Edit variable metadata
            names(categories(ds$v4)) <- c("d", "e")
            name(ds$v2) <- "Variable Two"
            description(ds$v3) <- "The third variable in the dataset"
            
            # 2. Edit dataset metadata
            description(ds) <- "A dataset for testing"
            
            # 3. Reorder variables
            ordering(ds) <- VariableOrder(VariableGroup("Even", ds[c(2,4,6)]),
                VariableGroup("Odd", ds[c(1,3,5)]))
            
            # 4. Derive variable
            ds$v7 <- ds$v3 - 6
            
            ## Assert those things
            test_that("The edits are made", {
                expect_identical(names(categories(ds$v4)),
                    c("d", "e"))
                expect_identical(name(ds$v2), "Variable Two")
                expect_identical(description(ds$v3),
                    "The third variable in the dataset")
                expect_identical(description(ds), "A dataset for testing")
                expect_identical(as.vector(ds$v7), df$v3 - 6)
                expect_identical(aliases(variables(ds)), 
                    paste0("v", c(2,4,6,1,3,5,7)))
            })
            
            # print(versions(ds))
            ## Save a version
            try(saveVersion(ds, "My changes"))
            test_that("There are now two versions", {
                expect_identical(length(versions(ds)), 2L)
                expect_identical(names(versions(ds))[1], "My changes")
            })
            
            ## Revert to the first version
            # ds <- try(restoreVersion(ds, "initial import"))
            ## ^ fails occasionally bc of https://www.pivotaltracker.com/story/show/98580938
            ds <- try(restoreVersion(ds, 2))
            test_that("Restoring restored correctly", {
                expect_identical(length(versions(ds)), 1L)
                validImport(ds)
            })
            test_that("Added variables are really removed by rolling back", {
                ## This was user-reported: Order was reverted but derived 
                ## variables persisted, and by assigning an empty order, you can
                ## recover them.
                ordering(ds) <- VariableOrder()
                expect_identical(names(ds), names(df))
            })
        })
    })
}