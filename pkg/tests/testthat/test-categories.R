context("Categories")

with(fake.HTTP, {
    ds <- loadDataset("test ds")
    cats <- categories(ds$gender)

    test_that("category init", {
        expect_true(inherits(cats[[1]], "Category"))
        expect_true(is.category(cats[[1]]))
        expect_true(inherits(cats, "Categories"))
        expect_true(is.categories(cats))
        expect_identical(length(cats), 3L)
    })
    
    test_that("Categories validation", {
        expect_error(Categories(list(
            list(id=-1L, name="B", numeric_value=1L, missing=FALSE),
            list(id=2L, name="C", numeric_value=2L, missing=FALSE),
            list(id=-1L, name="No Data", numeric_value=NULL, missing=TRUE)
        )), "Invalid category ids: must be unique")
        expect_error(Categories(list(
            list(id=1L, name="Name 1", numeric_value=1L, missing=FALSE),
            list(id=2L, name="Name 1", numeric_value=2L, missing=FALSE),
            list(id=-1L, name="No Data", numeric_value=NULL, missing=TRUE)
        )), "Invalid category names: must be unique")
    })

    test_that("category slicers", {
        expect_true(is.categories(cats[1]))
    })
    
    test_that("categories toJSON", {
        frj <- function (...) fromJSON(..., simplifyWithNames=FALSE)
        expect_identical(cats, Categories(frj(toJSON(cats))))
        expect_identical(cats[1], Categories(frj(toJSON(cats[1]))))
        expect_identical(cats[[1]], Category(frj(toJSON(cats[[1]]))))
    })
    
    test_that("category getters", {
        male <- cats[[1]]
        expect_identical(name(male), "Male")
        expect_identical(value(male), 1)
        expect_identical(id(male), 1)
        expect_identical(names(cats), c("Male", "Female", "No Data"))
        expect_identical(values(cats), c(1, 2, NA))
        expect_identical(ids(cats), c(1, 2, -1))
    })

    test_that("category setters", {
        male <- cats[[1]]
        name(male) <- "uomo"
        expect_identical(name(male), "uomo")
        expect_true(is.category(male))
        value(male) <- 42
        expect_identical(value(male), 42)
        expect_error(id(male) <- 4)
        expect_error(value(male) <- "foo")
        expect_true(is.category(male))
    })

    test_that("categories setters", {
        new_names <- c("masculino", "femenino", "No Data")
        names(cats) <- new_names
        expect_true(is.categories(cats))
        expect_equal(names(cats), new_names)
        names(cats)[2] <- "donne"
        expect_true(is.categories(cats))
        expect_equal(names(cats)[1:2], c("masculino", "donne"))
    })

    test_that("dichotomize", {
        male <- cats[[1]]
        expect_false(is.selected(male))
        male$selected <- TRUE
        expect_true(is.selected(male))
        expect_equal(name(male), "Male")

        expect_false(is.dichotomized(cats))
        dcats <- dichotomize(cats, 1)
        expect_true(is.dichotomized(dcats))
        expect_true(is.selected(dcats[[1]]))
        expect_false(is.selected(dcats[[2]]))
        expect_equal(name(dcats[[1]]), "Male")
        expect_equal(names(dcats), c("Male", "Female", "No Data"))

        dcats2 <- dichotomize(cats, "Female")
        expect_true(is.dichotomized(dcats2))
        expect_false(is.selected(dcats2[[1]]))
        expect_true(is.selected(dcats2[[2]]))

        expect_error(dichotomize(cats, "Cat!"))

        cats2 <- undichotomize(dcats)
        expect_false(is.dichotomized(cats2))
        expect_false(is.selected(cats2[[1]]))
    })
    
    test_that("is.na", {
        expect_identical(is.na(cats), structure(c(FALSE, FALSE, TRUE), 
            .Names=c("Male", "Female", "No Data")))
        expect_true(is.na(cats[[3]]))
        expect_false(is.na(cats[[1]]))
    })

    test_that("na.omit", {
        expect_identical(length(cats), 3L)
        expect_identical(length(na.omit(cats)), 2L)
        expect_true(is.categories(na.omit(cats)))
        expect_true(all(vapply(na.omit(cats), is.category, logical(1))))
    })
})



if (run.integration.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            test_that("categories setters persist to the server", {
                expect_equal(names(categories(ds$v4)), c("B", "C"))
                names(categories(ds$v4))[1] <- "V"
                expect_equal(names(categories(ds$v4)), c("V", "C"))
                expect_identical(names(categories(ds$v4)),
                    names(categories(refresh(ds)$v4)))
                
                categories(ds$v4) <- categories(ds$v4)[2:1]
                expect_equal(names(categories(ds$v4)), c("C", "V"))
            })
            test_that("categories<- with invalid input gives helpful message", {
                expect_error(categories(ds$v4) <- 1:3,
                    "`categories(x) <- value` only accepts Categories, not numeric. Did you mean `values(categories(x)) <- value`?",
                    fixed=TRUE)
                expect_error(categories(ds$v4) <- c("A", "B", "C"),
                    "`categories(x) <- value` only accepts Categories, not character. Did you mean `names(categories(x)) <- value`?",
                    fixed=TRUE)
                expect_error(categories(ds$v4) <- list(),
                    "`categories(x) <- value` only accepts Categories, not list.",
                    fixed=TRUE)
                expect_error(categories(ds$v1) <- 1:3,
                    "category assignment not defined for NumericVariable")
            })
        })
    })
}