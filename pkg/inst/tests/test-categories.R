context("Categories")

cats <- vars$gender$body$categories

test_that("category init", {
    expect_true(inherits(Category(cats[[1]]), "Category"))
    expect_true(is.category(Category(cats[[1]])))
    expect_true(inherits(Categories(cats), "Categories"))
    expect_true(is.categories(Categories(cats)))
})

Cats <- Categories(cats)

test_that("category getters", {
    male <- Cats[[1]]
    expect_identical(name(male), cats[[1]][[CATEGORY_NAME_MAP[["name"]]]])
    expect_identical(value(male), cats[[1]][[CATEGORY_NAME_MAP[["value"]]]])
    expect_identical(names(Cats), selectFrom(CATEGORY_NAME_MAP[["name"]], cats))
    expect_identical(values(Cats), selectFrom(CATEGORY_NAME_MAP[["value"]], cats))
})

## to do: 
### category setters
### add Categories init to CategoricalVariable init
### update categorical methods as needed (e.g. summary)