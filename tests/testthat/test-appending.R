context("Append datasets")

test_that("crunchTimeout", {
    with(temp.option(crunch.timeout=7),
        expect_identical(crunchTimeout(), 7))
    with(temp.option(crunch.timeout=NULL),
        expect_identical(crunchTimeout(), 900))
    with(temp.option(crunch.timeout=list()),
        expect_identical(crunchTimeout(), 900))
})

test_that("askForPermission says no if not interactive", {
    expect_false(askForPermission())
})

with_mock_HTTP({
    ds <- loadDataset("test ds")
    test_that("Cannot append dataset to itself", {
        expect_error(appendDataset(ds, ds),
            "Cannot append dataset to itself")
    })
})

## Append with a fork
appendDataset <- function (d1, d2, ...) {
    f <- forkDataset(d1)
    on.exit(delete(f))

    fprime <- crunch::appendDataset(f, d2, ...)
    d1 <- mergeFork(d1, fprime)
    return(d1)
}

with_test_authentication({
    with(test.dataset(df, "part1"), {
        cats <- categories(part1$v4)
        with(test.dataset(df, "part2"), {
            v3.1 <- as.vector(part1$v3)
            v3.2 <- as.vector(part2$v3)
            test_that("Setup for appending identical datasets", {
                expect_true(is.numeric(v3.1))
                expect_true(is.numeric(v3.2))
                expect_equivalent(v3.1, df$v3)
                expect_equivalent(v3.2, df$v3)
                expect_identical(dim(part1), dim(part2))
                expect_identical(dim(part1), dim(df))
                expect_length(batches(part1), 2)
                expect_length(batches(part2), 2)
            })
            out <- appendDataset(part1, part2)
            test_that("append handles two identical Datasets", {
                expect_true(is.dataset(out))
                expect_identical(self(out), self(part1))
                expect_length(batches(out), 3)
                expect_identical(dim(out), c(nrow(df)*2L, ncol(df)))
                expect_identical(getNrow(out), nrow(df)*2L)
                expect_identical(nrow(out), length(as.vector(out$v3)))
                expect_identical(categories(out$v4), cats)
                expect_equivalent(as.vector(out$v3), rep(df$v3, 2))
                expect_identical(as.vector(out$v3), c(v3.1, v3.2))
            })

            try(crDELETE(urls(batches(out))[2]))
            out <- refresh(out)
            test_that("deleting a batch drops its rows", {
                expect_true(is.dataset(out))
                expect_length(batches(out), 2)
                expect_identical(dim(out), dim(df))
                expect_identical(categories(out$v4), cats)
                expect_equivalent(as.vector(out$v3), df$v3)
            })
        })
    })

    with(test.dataset(df[,2:5], "part1"), {
        cats <- categories(part1$v4)
        with(test.dataset(df[,1:3], "part2"), {
            p1.batches <- batches(part1)
            test_that("append handles missing variables from each", {
                out <- appendDataset(part1, part2)
                expect_length(refresh(p1.batches), 3)
                expect_identical(ncol(out), 5L)
                expect_identical(ncol(out), length(allVariables(out)))
                expect_true(setequal(names(out), paste0("v", 1:5)))
                expect_identical(nrow(out), nrow(df) * 2L)
                expect_identical(categories(out$v4), cats)
                expect_equivalent(as.vector(out$v3), rep(df$v3, 2))
                expect_equivalent(as.vector(out$v1),
                    c(rep(NA, nrow(df)), df$v1))
                expect_length(as.vector(out$v5), 40)
                expect_length(as.vector(out$v4), 40)
                expect_equivalent(as.vector(out$v4)[1:20], df$v4)
                expect_equivalent(as.vector(out$v4),
                    factor(levels(df$v4)[c(df$v4,
                        factor(rep(NA_character_, nrow(df))))]))
                expect_equivalent(as.Date(as.vector(out$v5))[1:20], df$v5)
                expect_equivalent(as.Date(as.vector(out$v5)),
                    c(df$v5, rep(NA, nrow(df))))
            })
        })
    })

    test_that("Datetimes are correctly appended", {
        datetime1 <- data.frame(
            cat=factor(c("A", "B")),
            wave=as.Date(rep(c("2014-04-15", "2014-06-15"), 4)))
        d1 <- newDataset(datetime1)
        datetime2 <- data.frame(
            cat=factor(c("B", "C")),
            wave=as.Date(rep("2014-08-15", 4)))
        d2 <- newDataset(datetime2)
        out <- appendDataset(d1, d2)
        expect_length(batches(out), 3)
        expect_identical(nrow(out), 12L)
        expect_true(is.Datetime(out$wave))
        expect_equivalent(as.vector(out$wave),
            c(datetime1$wave, datetime2$wave))
    })

    lets <- LETTERS[1:5]
    cat1 <- data.frame(A=1, B=1:5, C=factor(c(2, 3, 1, 5, 4), labels=lets))
    cat2 <- data.frame(A=1, B=1:5, C=factor(c(2, 3, 1, 5, 4),
        labels=rev(lets)))
    with(test.dataset(cat1, "part1"), {
        with(test.dataset(cat2, "part2"), {
            test_that("Setup", {
                expect_identical(as.character(as.vector(part1$C)),
                    c("B", "C", "A", "E", "D"))
                c1 <- na.omit(categories(part1$C))
                expect_identical(names(c1), lets)
                expect_equivalent(values(c1), 1:5)
                expect_equivalent(ids(c1), 1:5)
                expect_identical(as.character(as.vector(part2$C)),
                    c("D", "C", "E", "A", "B"))
                c2 <- na.omit(categories(part2$C))
                expect_identical(names(c2), rev(lets))
                expect_equivalent(values(c2), 1:5)
                expect_equivalent(ids(c2), 1:5)
            })
            test_that("Categories with different ids and values line up by name", {
                out <- appendDataset(part1, part2)
                expect_identical(as.character(as.vector(out$C)),
                    c("B", "C", "A", "E", "D", "D", "C", "E", "A", "B"))
                cout <- na.omit(categories(out$C))
                ## Order comes from the "part1" dataset
                expect_identical(names(cout), lets)
                expect_equivalent(values(cout), 1:5)
                expect_equivalent(ids(cout), 1:5)
            })
        })
    })
})
