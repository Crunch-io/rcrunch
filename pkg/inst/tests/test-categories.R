context("Categories")

cats <- vars$gender$body$categories

test_that("category init", {
    expect_true(inherits(Category(cats[[1]]), "Category"))
    expect_true(is.category(Category(cats[[1]])))
    expect_true(inherits(Categories(cats), "Categories"))
    expect_true(is.categories(Categories(cats)))
    expect_identical(length(cats), 2L)
})

Cats <- Categories(cats)

test_that("category slicers", {
    expect_true(is.categories(Cats[1]))
})

test_that("category getters", {
    male <- Cats[[1]]
    expect_identical(name(male), cats[[1]][[CATEGORY_NAME_MAP[["name"]]]])
    expect_identical(value(male), cats[[1]][[CATEGORY_NAME_MAP[["value"]]]])
    expect_identical(id(male), cats[[1]][[CATEGORY_NAME_MAP[["id"]]]])
    expect_identical(names(Cats), selectFrom(CATEGORY_NAME_MAP[["name"]], cats))
    expect_identical(values(Cats), selectFrom(CATEGORY_NAME_MAP[["value"]], cats))
    expect_identical(ids(Cats), selectFrom(CATEGORY_NAME_MAP[["id"]], cats))
    expect_identical(length(ids(Cats)), 2L)
})

test_that("categories toJSON", {
    frj <- function (...) fromJSON(..., simplifyWithNames=FALSE)
    expect_identical(cats, frj(toJSON(Cats)))
    expect_identical(cats[1], frj(toJSON(Cats[1])))
    expect_identical(cats[[1]], frj(toJSON(Cats[[1]])))
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
    new_names <- c("masculino", "femenino")
    names(Cats) <- new_names
    expect_true(is.categories(Cats))
    expect_equal(names(Cats), new_names)
    names(Cats)[2] <- "donne"
    expect_true(is.categories(Cats))
    expect_equal(names(Cats), c("masculino", "donne"))
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