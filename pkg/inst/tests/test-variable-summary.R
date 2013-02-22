context("Variable summaries")

test_that("makeCategoricalTable", {
    testtable <- makeCategoricalTable(vars$gender$body$categories, sums$gender$body$categories)
    expect_true(is.table(testtable))
    expect_identical(length(testtable), 2L)
    expect_identical(names(testtable), 
        selectFrom("name", vars$gender$body$categories))
})

v1 <- as.variable(vars$gender)
v1@urls$summary_url <- sums$gender

test_that("CategoricalVariable.table", {
    expect_true(is.table(CategoricalVariable.table(v1)))
    expect_identical(names(CategoricalVariable.table(v1)), 
        selectFrom("name", categories(v1)))
})

test_that("table method?", {
    testtable <- makeCategoricalTable(vars$gender$body$categories, sums$gender$body$categories)
    expect_identical(testtable, table(v1))
})