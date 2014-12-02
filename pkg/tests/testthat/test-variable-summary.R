context("Variable summaries")

with(fake.HTTP, {
    ds2 <- loadDataset("test ds")
    gen <- ds2$gender
    tablecats <- categories(gen)
    tablesums <- crGET(gen@urls$summary_url)$categories

    test_that("ids getter for summaries", {
        expect_equivalent(ids(tablesums), selectFrom("id", tablesums))
        expect_true(setequal(ids(tablecats), ids(tablesums)))
    })

    test_that("makeCategoricalTable", {
        testtable <- makeCategoricalTable(tablesums)
        expect_true(is.table(testtable))
        expect_identical(length(testtable), 2L)
        expect_identical(names(testtable), names(na.omit(tablecats)))
    })

    
    test_that("CategoricalVariable.table", {
        expect_true(is.table(CategoricalVariable.table(gen)))
        expect_identical(names(CategoricalVariable.table(gen)),
            names(na.omit(categories(gen))))
    })

    test_that("options in CategoricalVariable.table", {
        expect_identical(length(CategoricalVariable.table(gen, useNA="no")),
            2L)
        expect_identical(length(CategoricalVariable.table(gen, useNA="ifany")),
            2L)
        expect_identical(length(CategoricalVariable.table(gen, useNA="always")),
            3L)
        expect_true(is.table(CategoricalVariable.table(gen, useNA="always")))
        ## Now see what happens if there are missing values
        gen@urls$summary_url <- sub("summary", "summary_with_missing",
            gen@urls$summary_url)
        expect_identical(length(CategoricalVariable.table(gen, useNA="no")),
            2L)
        expect_identical(length(CategoricalVariable.table(gen, useNA="ifany")),
            3L)
        expect_identical(length(CategoricalVariable.table(gen, useNA="always")),
            3L)
    })

    test_that("table 'method' dispatch", {
        testtable <- makeCategoricalTable(tablesums)
        expect_identical(testtable, table(gen))
        expect_identical(CategoricalVariable.table(gen, useNA="ifany"), 
            table(gen, useNA="ifany"))
        expect_identical(table(1:5), base::table(1:5))
        expect_identical(table(useNA="ifany", 1:5), 
            base::table(useNA="ifany", 1:5))
        expect_identical(table(useNA="ifany", c(NA, 1:5)),
            base::table(useNA="ifany", c(NA, 1:5)))
        expect_identical(testtable, table(useNA="no", gen))
    })

    test_that("unsupported table methods", {
        expect_error(table(gen, gen), 
            "Cannot currently tabulate more than one Crunch variable")
        expect_error(table(gen, 1:5), 
            "Cannot currently tabulate Crunch variables with non-Crunch vectors")
        expect_error(table(1:5, gen), 
            "Cannot currently tabulate Crunch variables with non-Crunch vectors")
        expect_error(table(), "nothing to tabulate")
    })
})

if (run.integration.tests) {
    with(test.authentication, {
        with(test.dataset(df, "testdf"), {
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
                expect_identical(table(testdf$v4), table(df$v4))
            })
            test_that("summary", {
                expect_equivalent(round(unclass(summary(testdf$v1)), 2),
                    round(unclass(summary(df$v1)), 2))
                expect_equivalent(as.numeric(summary(testdf$v4)), summary(df$v4))
            })
        })
    })
}