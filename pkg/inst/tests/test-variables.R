context("Variables")

test_that("Subclass constructor selector", {
    expect_equivalent(class(pickSubclassConstructor("numeric")), 
        "classGeneratorFunction")
    expect_identical(pickSubclassConstructor("numeric"), NumericVariable)
    expect_identical(pickSubclassConstructor("categorical"), CategoricalVariable)
    expect_identical(pickSubclassConstructor("text"), TextVariable)
    expect_identical(pickSubclassConstructor("datetime"), DatetimeVariable)
    expect_identical(pickSubclassConstructor("multiple_response"),
        MultipleResponseVariable)
    expect_identical(pickSubclassConstructor(), CrunchVariable)
    expect_identical(pickSubclassConstructor("foo"), CrunchVariable)
})

with(fake.HTTP, {
    ds <- as.dataset(GET("api/datasets/dataset1.json"))
    
    test_that("Variable init, as, is", {
        expect_true(is.variable(ds[[1]]))
        expect_true(all(vapply(ds, is.variable, logical(1))))
        expect_false(is.variable(5))
        expect_false(is.variable(NULL))
    })
    
    test_that("Variable subclass definitions, is", {
        expect_true(is.dataset(ds))
        expect_true(is.Categorical(ds$gender))
        expect_true(is.Numeric(ds$birthyr))
        expect_true(is.Text(ds[["textVar"]]))
        expect_true(is.Datetime(ds$starttime))
        expect_true(is.Multiple(ds$mymrset))
    })
    
    test_that("Categories", {
        thisone <- categories(ds$gender)
        expect_true(is.categories(thisone))
        expect_identical(length(thisone), 3L)

        expect_true(is.category(thisone[[1]]))
        expect_identical(categories(ds$birthyr), NULL)
    })
    
    test_that("Tuple metadata", {
        expect_identical(name(ds$gender), "Gender")
        expect_identical(description(ds$starttime), "Interview Start Time")
    })
    
    test_that("refresh", {
        expect_identical(ds$gender, refresh(ds$gender))
    })
})

