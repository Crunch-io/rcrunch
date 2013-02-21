context("Variable summaries")

test_that("makeCategoricalTable", {
    testtable <- makeCategoricalTable(vars$gender$body$categories, sums$gender$body$categories)
    expect_true(is.table(testtable))
    expect_identical(length(testtable), 2L)
    expect_identical(names(testtable), 
        selectFrom("name", vars$gender$body$categories))
})