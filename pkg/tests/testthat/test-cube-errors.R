context("Cube error handling")

test_that("'formula' must be provided", {
    expect_error(getCube(), "Must provide a formula")
})

test_that("formula must be a valid formula", {
    expect_error(getCube("asdf"),
        paste0(dQuote("formula"), " is not a valid formula"))
})

test_that("formula '.' argument is not permitted", {
    expect_error(getCube(~ ., data=ds),
        paste("getCube does not support", dQuote("."), "in formula"))
})

test_that("formula must have variables", {
    expect_error(getCube("~"),
        paste0(dQuote("formula"), " is not a valid formula"))
    expect_error(getCube(~ 1),
        "Must supply one or more variables")
})

test_that("'data' must be a Dataset", {
    expect_error(getCube(~ a), paste(dQuote("data"), "must be a Dataset"))
    ## Support a case of data=missing, i.e. eval formula as is?
    expect_error(getCube(~ a, data=NULL), 
        paste(dQuote("data"), "must be a Dataset"))
})

if (run.integration.tests) {
    with(test.authentication, {
        with(test.dataset(df[,1:4]), {
            test_that("All variables must be present in data", {
                expect_error(getCube(~ aaa + v3, data=ds),
                    paste0(dQuote("aaa"), " is not found in ",
                    dQuote("data")))
                expect_error(getCube(~ aaa + bbb, data=ds),
                    paste0(dQuote("aaa"), " and ", dQuote("bbb"),
                    " are not found in ", dQuote("data")))
                ## But works if variable is in workspace
                aaa <- ds$v4
                expect_equivalent(cubeToArray(getCube(~ aaa + bin(v3),
                    data=ds)),
                    array(c(1, 1, 3, 2, 2, 3, 3, 2, 1, 2), dim=c(2L, 5L),
                        dimnames=list(v4=c("B", "C"),
                            v3=c("5-10", "10-15", "15-20", "20-25", "25-30"))))
                ## What about a combination of a Variable and a local R vector?
            })
            
            test_that("Reserved function names cannot be variable aliases", {
                dsb <- ds
                al <- aliases(variables(dsb))
                dsb@variables@index[[which(al == "v1")]]$alias <- "mean"
                dsb@variables@index[[which(al == "v2")]]$alias <- "sd"

                expect_error(getCube(~ mean + bin(v3), data=dsb),
                    paste0("Cannot evaluate a cube with reserved name: ",
                    dQuote("mean")))
                expect_error(getCube(~ mean + sd, data=dsb),
                    paste0("Cannot evaluate a cube with reserved names: ",
                    dQuote("mean"), " and ", dQuote("sd")))
                ## But you can still get analyses with other variables
                expect_equivalent(cubeToArray(getCube(~ bin(v3), data=dsb)),
                    array(c(2, 5, 5, 5, 3), dim=c(5L),
                        dimnames=list(v3=c("5-10", "10-15", "15-20", "20-25",
                        "25-30"))))
            })
            
            test_that("What happens if 'measures' are not aggregations?", {
                
            })
            
            test_that("What happens if there are more than one vars on LHS?", {
                
            })
            
            test_that("What happens if there are no RHS vars?", {
                
            })
            
            test_that("What if there are aggregations on the RHS?", {
                
            })
            
            test_that("'rollup' on non-Datetime", {
                
            })
            
            test_that("Invalid rollup resolution", {
                
            })
            
            test_that("Unsupported aggregation functions", {
                
            })
            
            test_that("Limit on number of dimension variables", {
                
            })
        })
    })
}