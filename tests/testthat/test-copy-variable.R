context("Shallow copies of variables")

with(fake.HTTP, {
    ds <- loadDataset("test ds")
    
    expect_error(copy(ds$gender), 
        'Error : POST /api/datasets/dataset1/variables.json {"name":"Gender (copy)","expr":{"function":"copy_variable","args":[{"variable":"66ae9881e3524f7db84970d556c34552"}]}}\n',
        fixed=TRUE)
})

if (run.integration.tests) {
    with(test.authentication, {
        with(test.dataset(newDatasetFromFixture("apidocs")), {
            q1_url <- self(ds$q1)
            varcat_url <- self(variables(ds))
            test_that("Can copy and manipulate a categorical variable", {
                expect_false("copy1" %in% names(ds))
                expect_true("q1" %in% names(ds))
                q1_copy <- copy(ds$q1, name="copy1", alias="copy1")
                expect_identical(as.vector(q1_copy), as.vector(ds$q1))
                expect_false(name(q1_copy) == name(ds$q1))
                expect_false(alias(q1_copy) == alias(ds$q1))
                expect_false(self(q1_copy) == self(ds$q1))
                ds <- refresh(ds)
                expect_true("copy1" %in% names(ds))
                expect_true("q1" %in% names(ds))
                
                skip("Can't edit categories in copy")
                ## Edit category in copy
                names(categories(ds$copy1))[2] <- "Canine"
                expect_identical(names(categories(ds$copy1))[1:3], 
                    c("Cat", "Canine", "Bird"))
                expect_identical(names(categories(ds$q1))[1:3], 
                    c("Cat", "Dog", "Bird"))
                                
                ## Edit categories in original
                categories(ds$q1)[1:2] <- categories(ds$q1)[2:1]
                expect_identical(names(categories(ds$copy1))[1:3], 
                    c("Cat", "Canine", "Bird"))
                expect_identical(names(categories(ds$q1))[1:3], 
                    c("Dog", "Cat", "Bird"))  
            })
            
            test_that("Can copy and assign into dataset", {
                expect_true("q1" %in% names(ds))
                expect_true(is.Categorical(ds$q1))
                ds$q1_copy <- copy(ds$q1, name="copy2", alias="copy2")
                expect_true("q1_copy" %in% names(ds))
                expect_true(is.Categorical(ds$q1_copy))
                expect_true("q1" %in% names(ds))
                expect_true(is.Categorical(ds$q1))
                expect_identical(as.vector(ds$q1_copy), as.vector(ds$q1))
                expect_false(name(ds$q1_copy) == name(ds$q1))
                expect_false(alias(ds$q1_copy) == alias(ds$q1))
            })
            
            test_that("Can copy an array variable and manipulate it independently", {
                ds$allpets2 <- copy(ds$allpets)
                expect_true("allpets" %in% names(ds))
                expect_true("allpets2" %in% names(ds))
                expect_identical(name(ds$allpets2), "All pets owned (copy)")
                name(ds$allpets2) <- "Copy of allpets"
                expect_identical(name(ds$allpets2), "Copy of allpets")
                expect_identical(name(ds$allpets), "All pets owned")
                
                ## Edit subvariables in the copy
                subvariables(ds$allpets2)[1:2] <- subvariables(ds$allpets2)[2:1]
                expect_identical(names(subvariables(ds$allpets2)), 
                    c("Dog", "Cat", "Bird"))
                expect_identical(names(subvariables(ds$allpets)), 
                    c("Cat", "Dog", "Bird"))
                
                ## Edit subvariable names in the original
                names(subvariables(ds$allpets))[2] <- "Canine"
                expect_identical(names(subvariables(ds$allpets2)), 
                    c("Dog", "Cat", "Bird"))
                expect_identical(names(subvariables(ds$allpets)), 
                    c("Cat", "Canine", "Bird"))
            })
            
            test_that("Can copy subvariables (as non-subvars)", {
                
            })
            
            test_that("Can make a new array using copies of other array's subvars", {
                
            })
        })
    })
}