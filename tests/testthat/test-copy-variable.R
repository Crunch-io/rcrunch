context("Shallow copies of variables")

with(fake.HTTP, {
    ds <- loadDataset("test ds")
    
    expect_error(copy(ds$gender), 
        'Error : POST /api/datasets/dataset1/variables.json {"name":"Gender (copy)","expr":{"variable":"66ae9881e3524f7db84970d556c34552"}}\n',
        fixed=TRUE)
})

if (run.integration.tests) {
    with(test.authentication, {
        with(test.dataset(newDatasetFromFixture("apidocs")), {
            q1_url <- self(ds$q1)
            varcat_url <- self(variables(ds))
            test_that("Can copy a categorical variable", {
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
            
            test_that("Copying makes unique names and aliases", {
                
            })
            
            test_that("Can copy an array variable", {
                
            })
            
            test_that("Can copy subvariables (as non-subvars)", {
                
            })
            
            test_that("Can make a new array using copies of other array's subvars", {
                
            })
        })
    })
}