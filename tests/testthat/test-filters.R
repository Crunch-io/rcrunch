context("Filters")

with(fake.HTTP, {
    
})

if (run.integration.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            test_that("We have an empty filter catalog", {
                expect_true(inherits(filters(ds), "FilterCatalog"))
                expect_identical(length(filters(ds)), 0L)
            })
            
            test_that("We can create a filter", {
                filters(ds)[["Test filter"]] <- ds$v4 == "B"
                expect_identical(length(filters(ds)), 1L)
                expect_identical(names(filters(ds)), "Test filter")
                skip("TODO implement entities")
                expect_identical(name(filters(ds)[[1]]), "Test filter")
            })
            
            test_that("We have an applied filters view", {
                expect_identical(length(appliedFilters(ds)), 0L)
            })
            
            test_that("We can 'apply' a filter", {
                appliedFilters(ds) <- filters(ds)[["Test filter"]]
                expect_identical(length(appliedFilters(ds)), 1L)
            })
            
            test_that("'applied filters' for the UI don't affect R", {
                expect_identical(length(appliedFilters(ds)), 1L)
                validImport(ds)
            })
            
            test_that("We also have 'active filter' for the R object", {
                expect_true(inherits(activeFilter(ds), "CrunchLogicalExpr"))
                expect_identical(zcl(activeFilter(ds)), list())
            })
            
            test_that("We can set 'active filter'", {
                activeFilter(ds) <- ds$v4 == "C"
                expect_identical(zcl(activeFilter(ds)), zcl(ds$v4 == "C"))
            })
            
            test_that("If we set an active filter, cubes will be filtered by it (and not UI filters)", {
                activeFilter(ds) <- ds$v4 == "C"
                expect_equivalent(as.array(crtabs(~ v4, data=ds)), 
                    array(c(0, 10), dim=2L, dimnames=list(v4=c("B", "C"))))
            })
        })
    })
}