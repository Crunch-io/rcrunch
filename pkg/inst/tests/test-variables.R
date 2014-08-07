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
    session_store$datasets <- DatasetCatalog(GET("api/datasets.json"))
    ds <- loadDataset("test ds")
    # ds <- as.dataset(GET("api/datasets/dataset1.json"))
    
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
        expect_true(is.Array(ds$mymrset))
        expect_false(is.CA(ds$mymrset))
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
        expect_identical(alias(ds$gender), "gender")
    })
    
    test_that("refresh", {
        expect_identical(ds$gender, refresh(ds$gender))
    })
})

if (!run.only.local.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            test_that("can delete variables", {
                expect_true("v1" %in% names(ds))
                d <- try(delete(ds$v1))
                expect_false(is.error(d))
                expect_false("v1" %in% names(refresh(ds)))
            })
        })
        
        with(test.dataset(df), {
            test_that("can modify names and descriptions", {
                name(ds$v1) <- "Variable 1"
                expect_identical(name(ds$v1), "Variable 1")
                description(ds$v2) <- "Description 2"
                expect_identical(description(ds$v2), "Description 2")
                ds <- refresh(ds)
                expect_identical(name(ds$v1), "Variable 1")
                expect_identical(description(ds$v2), "Description 2")
                
                v1 <- ds$v1
                name(v1) <- "alt"
                expect_identical(name(v1), "alt")
                v1 <- refresh(v1)
                expect_identical(name(v1), "alt")
                description(v1) <- "asdf"
                expect_identical(description(v1), "asdf")
                v1 <- refresh(v1)
                expect_identical(description(v1), "asdf")
                alias(v1) <- "Alias!"
                expect_identical(alias(v1), "Alias!")
                v1 <- refresh(v1)
                expect_identical(alias(v1), "Alias!")
            })
        })
    })
}