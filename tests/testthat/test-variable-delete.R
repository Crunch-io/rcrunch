context("Deleting variables")

if (run.integration.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            test_that("deleteVariable(s)", {
                
            })
        })
        ## TODO: make deleteVariables fully delete arrays.
        
        with(test.dataset(df), {
            test_that("deleteVariables with consent", {
                
            })
        })
        
        with(test.dataset(df), {
            test_that("Delete variable by assigning NULL", {
                
            })
        })
    })
}