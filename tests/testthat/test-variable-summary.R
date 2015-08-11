context("Variable summaries")

with(fake.HTTP, {
    ds <- loadDataset("test ds")
    gen <- ds$gender

    test_that("table 'method' dispatch", {
        expect_identical(table(1:5), base::table(1:5))
        expect_identical(table(useNA="ifany", 1:5), 
            base::table(useNA="ifany", 1:5))
        expect_identical(table(useNA="ifany", c(NA, 1:5)),
            base::table(useNA="ifany", c(NA, 1:5)))
    })

    test_that("unsupported table methods", {
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
                expect_equivalent(table(testdf$v4), table(df$v4))
            })
            test_that("summary", {
                expect_equivalent(round(unclass(summary(testdf$v1)), 2),
                    round(unclass(summary(df$v1)), 2))
                expect_equivalent(as.numeric(summary(testdf$v4)),
                    summary(df$v4))
            })
        })
    })
}