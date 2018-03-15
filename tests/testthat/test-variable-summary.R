context("Variable summaries")

with_mock_crunch({
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
    
    test_that("table makes a cube request", {
        expect_GET(table(gen), "https://app.crunch.io/api/datasets/1/cube/")
    })
    
    test_that("unsupported aggregation methods", {
        expect_error(mean(ds$textVar),
                     paste(dQuote("mean"), "is not defined for TextVariable"))
        expect_error(sd(ds$textVar),
                     paste(dQuote("sd"), "is not defined for TextVariable"))
        expect_error(median(ds$textVar),
                     paste(dQuote("median"), "is not defined for TextVariable"))
        expect_error(min(ds$textVar),
                     paste(dQuote("min"), "is not defined for TextVariable"))
        expect_error(max(ds$textVar),
                     paste(dQuote("max"), "is not defined for TextVariable"))
    })
    
    test_that("Summary method for numeric", {
        expect_is(summary(ds$birthyr), "NumericVariableSummary")
    })
    test_that("max", {
        expect_equal(max(ds$birthyr), 1.6662)
    })
    test_that("min", {
        expect_equal(min(ds$birthyr), -1.4967)
    })
    test_that("length", {
        expect_identical(length(ds$birthyr), nrow(ds))
    })
})

with_test_authentication({
    ds <- newDataset(df)
    test_that("can fetch variable summaries", {
        summ <- getSummary(ds$v1)
        expect_true(is.list(summ))
        expect_equivalent(summ$mean, mean(df$v1, na.rm=TRUE))
        expect_equivalent(summ$stddev, sd(df$v1, na.rm=TRUE))
    })
    expect_stats_equal <- function (var, expected, stats=c("mean", "sd", "median", "min", "max")) {
        for (stat in stats) {
            fn <- get(stat)
            expect_equal(fn(var), fn(expected), info=stat)
            expect_equal(fn(var, na.rm=TRUE), fn(expected, na.rm=TRUE), info=stat)
        }
    }
    test_that("Univariate statistics for numeric variable", {
        expect_true(is.Numeric(ds$v1))
        expect_stats_equal(ds$v1, df$v1)
    })
    test_that("Univariate statistics for datetime variable", {
        expect_true(is.Datetime(ds$v5))
        expect_stats_equal(ds$v5, df$v5, c("min", "max"))
    })
    test_that("table", {
        expect_equivalent(table(ds$v4), table(df$v4))
        expect_equivalent(table(ds$v4, ds$v3), table(df$v4, df$v3))
    })
    test_that("table works with CrunchExpr", {
        expect_equivalent(table(ds$v4[ds$v3 < 10]), table(df$v4[df$v3 < 10]))
    })
    test_that("table throws error if not equally filtered", {
        expect_error(table(ds$v4, ds$v2[ds$v3 < 10]),
                     "Filter expressions in variables must be identical")
    })
    test_that("summary", {
        expect_equivalent(round(unclass(summary(ds$v1)), 2),
                          round(unclass(summary(df$v1)), 2))
        expect_equivalent(as.numeric(summary(ds$v4)), summary(df$v4))
    })
    test_that("Filtering summary and univariate stats", {
        expect_stats_equal(ds$v1[4:15], df$v1[4:15])
        expect_stats_equal(ds$v5[4:15], df$v5[4:15], c("min", "max"))
        expect_equivalent(round(unclass(summary(ds$v1[4:15])), 2),
                          round(unclass(summary(df$v1[4:15])), 2))
        expect_equivalent(as.numeric(summary(ds$v4[4:15])), summary(df$v4[4:15]))
    })
})
