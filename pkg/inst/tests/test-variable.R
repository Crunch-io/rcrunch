context("Variables")

test_that("Variable init, as, is", {
    expect_true(is.variable(do.call("CrunchVariable", vars[[1]])))
    expect_true(is.variable(as.variable(vars[[1]])))
    expect_true(is.variable(as.variable(as.shojiObject(vars[[1]]))))
    expect_true(all(vapply(lapply(vars, as.variable), is.variable, logical(1))))
    expect_false(is.variable(5))
    expect_false(is.variable(NULL))
})

test_that("Subclass constructor selector", {
    expect_equivalent(class(pickSubclassConstructor("numeric")), 
        "classGeneratorFunction")
    expect_identical(pickSubclassConstructor("numeric"), NumericVariable)
    expect_identical(pickSubclassConstructor("categorical"), CategoricalVariable)
    expect_identical(pickSubclassConstructor("text"), TextVariable)
    expect_identical(pickSubclassConstructor(), CrunchVariable)
    expect_identical(pickSubclassConstructor("foo"), CrunchVariable)
})

v <- lapply(vars, as.variable)

test_that("Variable type method", {
    expect_identical(type(v[["age"]]), "numeric")
    expect_identical(type(v$gender), "categorical")
})

test_that("Variable subclass definitions, is", {
    expect_equivalent(class(v[["age"]]), "NumericVariable")
    expect_equivalent(class(v[["gender"]]), "CategoricalVariable")
    expect_equivalent(class(v[["textVar"]]), "TextVariable")
    expect_true(is.Numeric(v[["age"]]))
    expect_true(is.Categorical(v[["gender"]]))
    expect_true(is.Text(v[["textVar"]]))
})

if (!run.only.local.tests) {
    test_that("type casting and 'as'", {
        login(test.user)
            testdf <- loadDataset("making_a_dataset_from_df") 
                ## from previous test
            if (!is.Text(testdf[["v1"]])) type(testdf[["v1"]]) <- "text"
            testvar <- testdf[["v1"]]
        
            expect_true(is.Text(testvar))
            expect_true(is.Numeric(castVariable(testvar, "numeric")))
            expect_true(is.Text(castVariable(testvar, "text")))    
            type(testvar) <- "numeric"
            expect_true(is.Numeric(testvar))
            expect_false(is.Numeric(testdf[["v1"]])) 
            expect_true(is.Numeric(refresh(testdf[["v1"]])))
                ## since they're the same remote object
            
            expect_error(castVariable(, "foo"), 
                paste(sQuote("foo"), "is not a valid Crunch variable type."))
        logout()
    })
}