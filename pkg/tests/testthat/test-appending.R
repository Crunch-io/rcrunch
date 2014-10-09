context("Append datasets")

test_that("crunchTimeout", {
    opt <- getOption("crunch.timeout")
        options(crunch.timeout=7)
        expect_identical(crunchTimeout(), 7)
        options(crunch.timeout=NULL)
        expect_identical(crunchTimeout(), 60)
        options(crunch.timeout=list())
        expect_identical(crunchTimeout(), 60)        
    options(crunch.timeout=opt)
})

test_that("askForPermission says no if not interactive", {
    expect_false(askForPermission())
})

with(fake.HTTP, {
    ds <- loadDataset("test ds")
    test_that("Cannot append dataset to itself", {
        expect_error(appendDataset(ds, ds),
            "Cannot append dataset to itself")
    })
})

if (run.integration.tests) {
    with(test.authentication, {
        with(test.dataset(df, "part1"), {
            cats <- categories(part1$v4)
            with(test.dataset(df, "part2"), {
                p1.batches <- batches(part1)
                test_that("Batches before appending are right", {
                    expect_true(inherits(p1.batches, "ShojiCatalog"))
                    expect_identical(length(p1.batches), 1L)
                    expect_identical(length(batches(part2)), 1L)
                })
                out <- suppressMessages(try(addBatchToDataset(part1, part2)))
                test_that("can add batches to dataset", {
                    expect_true(is.character(out))
                    expect_true(grepl("/batches/", out))
                    expect_identical(length(batches(part1)), 2L)
                    expect_true(out %in% urls(batches(part1)))
                })
                status <- pollBatchStatus(out, batches(part1),
                    until="ready")
                test_that("batch status can be polled while we wait", {
                    expect_false(is.error(status))
                    expect_identical(status, "ready")
                })
            })
        })
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
                    expect_identical(length(batches(part1)), 1L)
                    expect_identical(length(batches(part2)), 1L)
                })
                out <- suppressMessages(try(appendDataset(part1, part2)))
                test_that("append handles two identical Datasets", {
                    expect_false(is.error(out))
                    expect_true(is.dataset(out))
                    expect_identical(self(out), self(part1))
                    expect_identical(length(batches(out)), 2L)
                    expect_identical(dim(out), c(nrow(df)*2L, ncol(df)))
                    expect_identical(getNrow(out), nrow(df)*2L)
                    expect_identical(nrow(out), length(as.vector(out$v3)))
                    expect_identical(categories(out$v4)[1:2], cats)
                    expect_equivalent(as.vector(out$v3), rep(df$v3, 2))
                    expect_identical(as.vector(out$v3), c(v3.1, v3.2))
                })
                
                try(DELETE(names(batches(out)@index)[2]))
                out <- refresh(out)
                test_that("deleting a batch drops its rows", {
                    expect_true(is.dataset(out))
                    expect_identical(length(batches(out)), 1L)
                    expect_identical(dim(out), dim(df))
                    expect_identical(categories(out$v4)[1:2], cats)
                    expect_equivalent(as.vector(out$v3), df$v3)
                })
            })
        })

        try({
            file1 <- newDatasetFromFile(testfile.csv, name=now())
            file2 <- newDatasetFromFile(testfile.csv, name=now())
            v3.1 <- as.vector(file1$V3)
            v3.2 <- as.vector(file2$V3)
            
            test_that("our assumptions about these two datasets from file", {
                expect_true(is.numeric(v3.1))
                expect_true(is.numeric(v3.2))
                expect_equivalent(v3.1, testfile.df$V3)
                expect_equivalent(v3.2, testfile.df$V3)
                expect_identical(length(batches(file1)), 1L)
                expect_identical(length(batches(file2)), 1L)
            })
            out <- suppressMessages(try(appendDataset(file1, file2)))
            test_that("append handles two identical Datasets from file", {
                expect_false(is.error(out))
                expect_true(is.dataset(out))
                expect_identical(self(out), self(file1))
                expect_identical(length(batches(out)), 2L)
                expect_identical(dim(out),
                    c(nrow(testfile.df)*2L, ncol(testfile.df)))
                expect_identical(getNrow(out), nrow(testfile.df)*2L)
                expect_identical(nrow(out), length(as.vector(out$V3)))
                expect_equivalent(as.vector(out$V3), rep(testfile.df$V3, 2))
                expect_identical(as.vector(out$V3), c(v3.1, v3.2))  
            })

            delete(file2)
            delete(file1)
        })
        
        with(test.dataset(df[,2:5], "part1"), {
            cats <- categories(part1$v4)
            with(test.dataset(df[,1:3], "part2"), {
                p1.batches <- batches(part1)
                test_that("if I insist on confirmation, it fails if there are conflicts", {
                    expect_true(inherits(p1.batches, "ShojiCatalog"))
                    expect_identical(length(p1.batches), 1L)
                    expect_error(suppressMessages(appendDataset(part1, part2,
                        confirm=TRUE)),
                        "Please manually resolve conflicts")
                    expect_identical(length(batches(part1)), 1L)
                })
                out <- suppressMessages(try(appendDataset(part1, part2)))
                test_that("append handles missing variables from each", {
                    expect_false(is.error(out))
                    expect_true(is.dataset(out))
                    expect_identical(length(refresh(p1.batches)), 2L)
                    expect_identical(ncol(out), 5L)
                    expect_identical(ncol(out), length(allVariables(out)))
                    expect_true(setequal(names(out), paste0("v", 1:5)))
                    expect_identical(nrow(out), nrow(df) * 2L)
                    expect_identical(categories(out$v4)[1:2], cats)
                    expect_equivalent(as.vector(out$v3), rep(df$v3, 2))
                    expect_equivalent(as.vector(out$v1), 
                        c(rep(NA, nrow(df)), df$v1))
                    expect_identical(length(as.vector(out$v5)), 40L)
                    expect_identical(length(as.vector(out$v4)), 40L)
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
        
        with(test.dataset(df[,1:3], "part1"), {
            with(test.dataset(df[,2:5], "part2"), {
                cats <- categories(part2$v4)
                p1.batches <- batches(part1)
                out <- suppressMessages(try(appendDataset(part1, part2)))
                test_that("append with missing variables the other way", {
                    expect_false(is.error(out))
                    expect_true(is.dataset(out))
                    expect_identical(length(refresh(p1.batches)), 2L)
                    expect_identical(ncol(out), 5L)
                    expect_identical(ncol(out), length(allVariables(out)))
                    expect_true(setequal(names(out), paste0("v", 1:5)))
                    expect_identical(nrow(out), nrow(df) * 2L)
                    expect_identical(categories(out$v4)[1:2], cats)
                    expect_equivalent(as.vector(out$v3), rep(df$v3, 2))
                    expect_equivalent(as.vector(out$v1), 
                        c(df$v1, rep(NA, nrow(df))))
                    expect_identical(length(as.vector(out$v5)), 40L)
                    expect_identical(length(as.vector(out$v4)), 40L)
                    expect_equivalent(as.vector(out$v4)[21:40], df$v4)
                    expect_equivalent(as.vector(out$v4), 
                        factor(levels(df$v4)[c(
                            factor(rep(NA_character_, nrow(df))), df$v4)]))
                    expect_equivalent(as.Date(as.vector(out$v5))[21:40], df$v5)
                    expect_equivalent(as.numeric(as.Date(as.vector(out$v5))), 
                        c(rep(NA, nrow(df)), df$v5))
                })
            })
        })
        
        with(test.dataset(df[,2:5], "part1"), {
            d2 <- df
            d2$v2 <- d2$v3 ## v2 was text, now is numeric
            with(test.dataset(d2, "part2"), {
                test_that("append fails on type mismatch", {
                    p1.batches <- batches(part1)
                    expect_true(inherits(p1.batches, "ShojiCatalog"))
                    expect_identical(length(p1.batches), 1L)
                    expect_error(appendDataset(part1, part2))
                    expect_identical(length(batches(part1)), 1L)
                })
            })
        })
        
        datetime1 <- data.frame(
            cat=factor(c("A", "B")),
            wave=as.Date(rep(c("2014-04-15", "2014-06-15"), 4)))
        datetime2 <- data.frame(
            cat=factor(c("B", "C")),
            wave=as.Date(rep("2014-08-15", 4)))
        with(test.dataset(datetime1, "part1"), {
            with(test.dataset(datetime2, "part2"), {
                test_that("setup for datetime appending", {
                    expect_true(is.Datetime(part1$wave))
                    expect_true(is.Datetime(part2$wave))
                })
                out <- suppressMessages(try(appendDataset(part1, part2)))
                test_that("Datetimes are correctly appended", {
                    expect_false(is.error(out))
                    expect_true(is.dataset(out))
                    expect_identical(length(batches(out)), 2L)
                    expect_identical(nrow(out), 12L)
                    expect_true(is.Datetime(out$wave))
                    expect_equivalent(as.vector(out$wave),
                        c(datetime1$wave, datetime2$wave))
                })
            })
        })
        
        sparse1 <- data.frame(A=factor(c("A", "B")), B=1:1000)
        sparse2 <- data.frame(B=1:1000, C=factor(c("C", "D")))
        with(test.dataset(sparse1, "part1"), {
            with(test.dataset(sparse2, "part2"), {
                out <- suppressMessages(try(appendDataset(part1, part2)))
                test_that("Datasets with more rows append (sparseness test)", {
                    expect_identical(mean(out$B), 1001/2)
                    expect_identical(length(as.vector(out$C)), 2000L)
                    expect_identical(as.vector(out$C), 
                        factor(c(rep(NA, 1000), rep(c("C", "D"), 500))))
                })
            })
        })
    })
}
