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
    expect_identical(pickSubclassConstructor("datetime"), DatetimeVariable)
    expect_identical(pickSubclassConstructor("multiple_response"),
        MultipleResponseVariable)
    expect_identical(pickSubclassConstructor(), CrunchVariable)
    expect_identical(pickSubclassConstructor("foo"), CrunchVariable)
})

v <- lapply(vars, as.variable)

test_that("Variable subclass definitions, is", {
    expect_equivalent(class(v[["age"]]), "NumericVariable")
    expect_equivalent(class(v[["gender"]]), "CategoricalVariable")
    expect_equivalent(class(v[["textVar"]]), "TextVariable")
    expect_equivalent(class(v$starttime), "DatetimeVariable")
    expect_equivalent(class(v$mymrset), "MultipleResponseVariable")
    expect_true(is.Numeric(v[["age"]]))
    expect_true(is.Categorical(v[["gender"]]))
    expect_true(is.Text(v[["textVar"]]))
    expect_true(is.Datetime(v$starttime))
    expect_true(is.Multiple(v$mymrset))
})

test_that("Categories", {
    expected <- Categories(vars$gender$body$categories)
    thisone <- categories(v[["gender"]])
    expect_true(is.categories(thisone))
    expect_identical(length(thisone), 3L)
    expect_identical(class(thisone), class(expected))
    expect_identical(length(thisone), length(expected))
    
    expect_identical(categories(v[["gender"]]), Categories(vars$gender$body$categories))
    expect_true(is.category(categories(v[["gender"]])[[1]]))
    expect_identical(categories(v[["age"]]), NULL)
})