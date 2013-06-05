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

## to do: 
### category setters