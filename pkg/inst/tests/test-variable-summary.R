context("Variable summaries")

test_that("makeCategoricalTable", {
    testtable <- makeCategoricalTable(vars$gender$body$categories,
        sums$gender$body$categories)
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

test_that("options in CategoricalVariable.table", {
    gen <- v1
    expect_identical(length(CategoricalVariable.table(gen, useNA="no")), 2L)
    expect_identical(length(CategoricalVariable.table(gen, useNA="ifany")), 3L)
    expect_identical(length(CategoricalVariable.table(gen, useNA="always")), 3L)
    expect_true(is.table(CategoricalVariable.table(gen, useNA="always")))
    gen@urls$summary_url$body$missing_count <- 0
    expect_identical(length(CategoricalVariable.table(gen, useNA="ifany")), 2L)
    expect_identical(length(CategoricalVariable.table(gen, useNA="always")), 3L)
})

test_that("table method", {
    testtable <- makeCategoricalTable(vars$gender$body$categories, sums$gender$body$categories)
    expect_identical(testtable, table(v1))
    expect_identical(CategoricalVariable.table(v1, useNA="ifany"), 
        table(v1, useNA="ifany"))
    expect_identical(table(1:5), base::table(1:5))
    expect_identical(table(useNA="ifany", 1:5), base::table(useNA="ifany", 1:5))
    expect_identical(table(useNA="ifany", c(NA, 1:5)), base::table(useNA="ifany", c(NA, 1:5)))
    
    expect_identical(testtable, table(useNA="no", v1))
})