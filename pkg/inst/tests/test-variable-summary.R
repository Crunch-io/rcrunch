context("Variable summaries")

tablecats <- Categories(vars$gender$body$categories)
tablesums <- sums$gender$categories

test_that("ids getter for summaries", {
    expect_identical(ids(tablesums), selectFrom("id", tablesums))
    expect_true(setequal(ids(tablecats), ids(tablesums)))
})

test_that("makeCategoricalTable", {
    testtable <- makeCategoricalTable(tablesums)
    expect_true(is.table(testtable))
    expect_identical(length(testtable), 2L)
    expect_identical(names(testtable), names(na.omit(tablecats)))
})

v1 <- as.variable(vars$gender)
v1@urls$summary_url <- sums$gender ## Injecting summary in, then use fake HTTP

with(fake.HTTP, {
    test_that("CategoricalVariable.table", {
        expect_true(is.table(CategoricalVariable.table(v1)))
        expect_identical(names(CategoricalVariable.table(v1)),
            names(na.omit(tablecats)))
    })

    test_that("options in CategoricalVariable.table", {
        gen <- v1
        expect_identical(length(CategoricalVariable.table(gen, useNA="no")),
            2L)
        expect_identical(length(CategoricalVariable.table(gen, useNA="ifany")),
            3L)
        expect_identical(length(CategoricalVariable.table(gen, useNA="always")),
            3L)
        expect_true(is.table(CategoricalVariable.table(gen, useNA="always")))
        gen@urls$summary_url$missing_count <- 0
        expect_identical(length(CategoricalVariable.table(gen, useNA="ifany")),
            2L)
        expect_identical(length(CategoricalVariable.table(gen, useNA="always")),
            3L)
    })

    test_that("table 'method' dispatch", {
        testtable <- makeCategoricalTable(tablesums)
        expect_identical(testtable, table(v1))
        expect_identical(CategoricalVariable.table(v1, useNA="ifany"), 
            table(v1, useNA="ifany"))
        expect_identical(table(1:5), base::table(1:5))
        expect_identical(table(useNA="ifany", 1:5), 
            base::table(useNA="ifany", 1:5))
        expect_identical(table(useNA="ifany", c(NA, 1:5)),
            base::table(useNA="ifany", c(NA, 1:5)))
        expect_identical(testtable, table(useNA="no", v1))
    })

    test_that("unsupported table methods", {
        expect_error(table(v1, v1), 
            "Cannot currently tabulate more than one Crunch variable")
        expect_error(table(v1, 1:5), 
            "Cannot currently tabulate Crunch variables with non-Crunch vectors")
        expect_error(table(1:5, v1), 
            "Cannot currently tabulate Crunch variables with non-Crunch vectors")
        expect_error(table(), "nothing to tabulate")
    })
})

if (!run.only.local.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            testdf <- .setup
            test_that("can fetch variable summaries", {
                summ <- getSummary(testdf$v1)
                expect_true(is.list(summ))
                expect_equivalent(summ$mean, mean(df$v1, na.rm=TRUE))
                expect_equivalent(summ$stddev, sd(df$v1, na.rm=TRUE))
            })
            test_that("method dispatch", {
                expect_identical(mean(testdf$v1), mean(df$v1))
                expect_equivalent(mean(testdf$v1, na.rm=TRUE), 
                    mean(df$v1, na.rm=TRUE))
                expect_identical(sd(testdf$v1), sd(testdf$v1))
                expect_equivalent(sd(testdf$v1, na.rm=TRUE), 
                    sd(testdf$v1, na.rm=TRUE))
                expect_identical(median(testdf$v1), median(testdf$v1))
                expect_identical(median(testdf$v1, na.rm=TRUE),
                    median(testdf$v1, na.rm=TRUE))
            })
            test_that("table", {
                ## This should be identical, not just equivalent.
                # print(str(table(testdf$v4)))
                # print(str(table(df$v4)))
                expect_equivalent(table(testdf$v4), table(df$v4))
            })
            test_that("summary", {
                # print(summary(testdf$v1)) ## breaking
                expect_equivalent(unclass(summary(testdf$v1)), unclass(summary(df$v1)))
                expect_equivalent(unclass(summary(testdf$v4)), summary(df$v4))
            })
        })
    })
}