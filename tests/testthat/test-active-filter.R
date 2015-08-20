context("Filtering datasets and variables in the R session")

with(fake.HTTP, {
    ds <- loadDataset("test ds")
    ds2 <- ds[ds$gender == "Male", ]
    ds3 <- ds2[ds$birthyr > 1981, ]
    
    test_that("[ method on dataset adds an active filter", {
        expect_identical(activeFilter(ds2), ds$gender == "Male")
    })
    test_that("Active filter persists on refreshing dataset", {
        expect_identical(activeFilter(refresh(ds2)), ds$gender == "Male")
    })
    test_that("Further [ on a filtered dataset ands the filters together", {
        expect_identical(activeFilter(ds3), 
            ds$gender == "Male" & ds$birthyr > 1981)
    })
    
    test_that("subset method for dataset does the same", {
        expect_identical(subset(ds, ds$gender == "Male"), ds2)
    })
    
    test_that("Variables extracted from a filtered dataset are also filtered", {
        expect_identical(activeFilter(ds2$birthyr), ds$gender == "Male")
        skip("Not really supporting this correctly")
        expect_identical(ds[ds$gender == "Male", "birthyr"], 
            ds$gender == "Male")
    })
    
    test_that("Subvariables extracted from a filtered array are also filtered", {
        mr <- ds2$mymrset
        expect_true(is.Multiple(mr))
        expect_identical(activeFilter(mr), ds$gender == "Male")
        sv1 <- subvariables(mr)[[1]]
        expect_true(is.Categorical(sv1))
        expect_identical(activeFilter(sv1), ds$gender == "Male")
    })
    
    test_that("Active filter persists on refreshing variable", {
        expect_identical(activeFilter(refresh(ds2$birthyr)), 
            ds$gender == "Male")
    })
    
    test_that("Getting weight variable from filtered dataset is filtered", {
        ds4 <- ds2
        ds4@body$weight <- "/api/datasets/dataset1/variables/starttime.json"
        expect_identical(weight(ds4), ds4$starttime)
        expect_identical(activeFilter(ds4$starttime), ds$gender == "Male")
    })
})

if (run.integration.tests) {
    with(test.authentication, {
        with(test.dataset(df), {            
            test_that("Filtered variables return filtered values from as.vector", {
                
            })
            
            test_that("as.data.frame when filtered", {
                
            })
            
            test_that("filtered dim", {
                
            })
            
            test_that("filtered cubing", {
                
            })
            
            test_that("filtered updating", {
                
            })
        })
    })
}