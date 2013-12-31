context("Categories")

cats <- vars$gender$body$categories

test_that("category init", {
    expect_true(inherits(Category(cats[[1]]), "Category"))
    expect_true(is.category(Category(cats[[1]])))
    expect_true(inherits(Categories(cats), "Categories"))
    expect_true(is.categories(Categories(cats)))
    expect_identical(length(cats), 3L)
})

Cats <- Categories(cats)

test_that("category slicers", {
    expect_true(is.categories(Cats[1]))
})

test_that("category getters", {
    male <- Cats[[1]]
    expect_identical(name(male), cats[[1]][["name"]])
    expect_identical(value(male), cats[[1]][["numeric_value"]])
    expect_identical(id(male), cats[[1]][["id"]])
    expect_identical(names(Cats), selectFrom("name", cats))
    expect_identical(values(Cats), selectFrom("numeric_value", cats))
    expect_identical(ids(Cats), selectFrom("id", cats))
    expect_identical(length(ids(Cats)), 3L)
})

test_that("categories toJSON", {
    frj <- function (...) fromJSON(..., simplifyWithNames=FALSE)
    expect_identical(Cats, Categories(frj(toJSON(Cats))))
    expect_identical(Cats[1], Categories(frj(toJSON(Cats[1]))))
    expect_identical(Cats[[1]], Category(frj(toJSON(Cats[[1]]))))
})

test_that("category setters", {
    male <- Cats[[1]]
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
    names(Cats) <- new_names
    expect_true(is.categories(Cats))
    expect_equal(names(Cats), new_names)
    names(Cats)[2] <- "donne"
    expect_true(is.categories(Cats))
    expect_equal(names(Cats)[1:2], c("masculino", "donne"))
})

test_that("dichotomize", {
    male <- Cats[[1]]
    expect_false(is.selected(male))
    male$selected <- TRUE
    expect_true(is.selected(male))
    expect_equal(name(male), "Male")
    
    expect_false(is.dichotomized(Cats))
    dCats <- dichotomize(Cats, 1)
    expect_true(is.dichotomized(dCats))
    expect_true(is.selected(dCats[[1]]))
    expect_false(is.selected(dCats[[2]]))
    expect_equal(name(dCats[[1]]), "Male")
    expect_equal(names(dCats), c("Male", "Female", "No Data"))
    
    dCats2 <- dichotomize(Cats, "Female")
    expect_true(is.dichotomized(dCats2))
    expect_false(is.selected(dCats2[[1]]))
    expect_true(is.selected(dCats2[[2]]))
  
    expect_error(dichotomize(Cats, "Cat!"))
    
    Cats2 <- undichotomize(dCats)
    expect_false(is.dichotomized(Cats2))
    expect_false(is.selected(Cats2[[1]]))
})

test_that("na.omit", {
    expect_identical(length(Cats), 3L)
    expect_identical(length(na.omit(Cats)), 2L)
    expect_true(is.categories(na.omit(Cats)))
    expect_true(all(vapply(na.omit(Cats), is.category, logical(1))))
})

if (!run.only.local.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            testdf <- .setup
            test_that("categories setters persist to the server", {
                expect_equal(names(categories(testdf$v4)), c("B", "C", "No Data"))
                names(categories(testdf$v4))[1] <- "V"
                expect_equal(names(categories(testdf$v4)), c("V", "C", "No Data"))
                expect_identical(names(categories(testdf$v4)),
                    names(categories(refresh(testdf)$v4)))
            })
        })
    })
}