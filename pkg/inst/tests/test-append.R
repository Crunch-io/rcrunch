context("Append datasets")

test_that("Conflict messages are formatted correctly", {
    c1 <- list()
    c2 <- list(var1=list(
        list(
            message="No good",
            resolution="But I fixed it already"
        )
    ))
    c3 <- list(
        var2=list(
            list(
                message="No good",
                resolution="But I fixed it already"
            )
        ),
        var1=list(
            list(
                message="No good",
                resolution="But I fixed it already"
            ), 
            list(
                message="Oh, and there was another problem",
                resolution="But it's also cool"
            )
        )
    )
    expect_equivalent(flattenConflicts(c3), 
        data.frame(
            message=c("No good", "No good", "Oh, and there was another problem"),
            resolution=c("But I fixed it already", "But I fixed it already", "But it's also cool"),
            url=c("var2", "var1", "var1"),
            stringsAsFactors=FALSE))
    
    expect_identical(formatConflicts(c1), "No conflicts.")
    expect_identical(formatConflicts(c2), 
        paste("Conflict: No good; Resolution: But I fixed it already; 1 variable:", dQuote("var1")))
    expect_identical(formatConflicts(c3), 
        c(paste("Conflict: No good; Resolution: But I fixed it already; 2 variables:", dQuote("var2"), "and", dQuote("var1")),
        paste("Conflict: Oh, and there was another problem; Resolution: But it's also cool; 1 variable:", dQuote("var1"))))
    
})

test_that("default.timeout", {
    opt <- getOption("crunch.timeout")
        options(crunch.timeout=7)
        expect_identical(default.timeout(), 7)
        options(crunch.timeout=NULL)
        expect_identical(default.timeout(), 60)
        options(crunch.timeout=list())
        expect_identical(default.timeout(), 60)        
    options(crunch.timeout=opt)
})

test_that("askForPermission says no if not interactive", {
    expect_false(askForPermission())
})

if (!run.only.local.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            part1 <- .setup
            cats <- categories(part1$v4)
            with(test.dataset(df), {
                part2 <- .setup
                test_that("can add batches to dataset", {
                    p1.batches <- batches(part1)
                    expect_true(inherits(p1.batches, "ShojiCatalog"))
                    expect_identical(length(p1.batches), 1L)
                    expect_identical(length(batches(part2)), 1L)
                    out <- try(addBatchToDataset(part1, part2))
                    expect_true(is.character(out))
                    expect_true(grepl("/batches/", out))
                    expect_identical(length(batches(part1)), 2L)
                    expect_true(out %in% names(batches(part1)@index))
                })
            })
        })
        with(test.dataset(df), {
            part1 <- .setup
            cats <- categories(part1$v4)
            with(test.dataset(df), {
                part2 <- .setup
                v3.1 <- as.vector(part1$v3)
                v3.2 <- as.vector(part2$v3)
                test_that("our assumptions about these two datasets", {
                    expect_true(is.numeric(v3.1))
                    expect_true(is.numeric(v3.2))
                    expect_equivalent(v3.1, df$v3)
                    expect_equivalent(v3.2, df$v3)
                    expect_identical(dim(part1), dim(part2))
                    expect_identical(dim(part1), dim(df))
                    expect_identical(length(batches(part1)), 1L)
                    expect_identical(length(batches(part2)), 1L)
                })
                test_that("append handles two identical Datasets", {
                    out <- try(appendDataset(part1, part2))
                    expect_false(is.error(out))
                    expect_true(is.dataset(out))
                    expect_identical(self(out), self(part1))
                    skip(expect_identical(length(batches(out)), 2L),
                        "3 != 2")
                    expect_identical(dim(out), c(nrow(df)*2L, ncol(df)))
                    expect_identical(getNrow(out), nrow(df)*2L)
                    expect_identical(nrow(out), length(as.vector(out$v3)))
                    expect_identical(categories(out$v4)[1:2], cats)
                    expect_equivalent(as.vector(out$v3), rep(df$v3, 2))
                    expect_identical(as.vector(out$v3), c(v3.1, v3.2))
                })
            })
        })

        file1 <- newDatasetFromFile(testfile.csv, name=now())
            file2 <- newDatasetFromFile(testfile.csv, name=now())
            try({
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
                
                test_that("append handles two identical Datasets from file", {
                    out <- try(appendDataset(file1, file2))
                    expect_false(is.error(out))
                    expect_true(is.dataset(out))
                    expect_identical(self(out), self(file1))
                    skip(expect_identical(length(batches(out)), 2L),
                        "3 != 2")
                    expect_identical(dim(out),
                        c(nrow(testfile.df)*2L, ncol(testfile.df)))
                    expect_identical(getNrow(out), nrow(testfile.df)*2L)
                    expect_identical(nrow(out), length(as.vector(out$V3)))
                    expect_equivalent(as.vector(out$V3), rep(testfile.df$V3, 2))
                    expect_identical(as.vector(out$V3), c(v3.1, v3.2))  
                })
            })
            delete(file2)
        delete(file1)
        
        with(test.dataset(df[,2:5]), {
            part1 <- .setup
            cats <- categories(part1$v4)
            with(test.dataset(df[,1:3]), {
                part2 <- .setup
                p1.batches <- batches(part1)
                test_that("if I insist on confirmation, it fails if there are conflicts", {
                    expect_true(inherits(p1.batches, "ShojiCatalog"))
                    expect_identical(length(p1.batches), 1L)
                    expect_error(appendDataset(part1, part2, confirm=TRUE))
                    expect_identical(length(batches(part1)), 1L)
                })
                test_that("append handles missing variables from each", {
                    out <- try(appendDataset(part1, part2))
                    expect_false(is.error(out))
                    expect_true(is.dataset(out))
                    skip(expect_identical(length(refresh(p1.batches)), 2L),
                        "3 != 2")
                    expect_identical(ncol(out), 5L)
                    expect_identical(ncol(out), length(out@variables))
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
        
        with(test.dataset(df[,1:3]), {
            part1 <- .setup
            with(test.dataset(df[,2:5]), {
                part2 <- .setup
                cats <- categories(part2$v4)
                
                test_that("append with missing variables the other way", {
                    p1.batches <- batches(part1)
                    out <- try(appendDataset(part1, part2))
                    expect_false(is.error(out))
                    expect_true(is.dataset(out))
                    skip(expect_identical(length(refresh(p1.batches)), 2L),
                        "3 != 2")
                    expect_identical(ncol(out), 5L)
                    expect_identical(ncol(out), length(out@variables))
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
        
        with(test.dataset(df[,2:5]), {
            part1 <- .setup
            d2 <- df
            d2$v2 <- d2$v3 ## v2 was text, now is numeric
            with(test.dataset(d2), {
                part2 <- .setup
                test_that("append fails on type mismatch", {
                    p1.batches <- batches(part1)
                    expect_true(inherits(p1.batches, "ShojiCatalog"))
                    expect_identical(length(p1.batches), 1L)
                    expect_error(appendDataset(part1, part2))
                    expect_identical(length(batches(part1)), 1L)
                })
            })
        })
        
        with(test.dataset(mrdf), {
            part1 <- .setup
            cast.these <- grep("mr_", names(part1))
            part1[cast.these] <- lapply(part1[cast.these],
                castVariable, "categorical")
            var1 <- makeMR(pattern="mr_[123]", dataset=part1,
                name="test1", selections="1.0")
            with(test.dataset(mrdf), {
                part2 <- .setup
                part2[cast.these] <- lapply(part2[cast.these],
                    castVariable, "categorical")
                var2 <- makeMR(pattern="mr_[123]", dataset=part2,
                    name="test1", selections="1.0")
                test_that("set up MR for appending", {
                    expect_true(is.Multiple(var1))
                    expect_true(is.Multiple(var2))
                    expect_identical(length(batches(part1)), 1L)
                    expect_identical(length(batches(part2)), 1L)
                })
                test_that("identical datasets with arrays can append", {
                    out <- try(appendDataset(part1, part2))
                    expect_false(is.error(out))
                    expect_true(is.dataset(out))
                    skip(expect_identical(length(batches(out)), 2L),
                        "3 != 2")
                    expect_identical(dim(out), c(nrow(mrdf)*2L, 2L))
                    expect_true(is.Multiple(out$test1))
                })
            })
        })
        
        with(test.dataset(mrdf), {
            part1 <- .setup
            cast.these <- grep("mr_", names(part1))
            part1[cast.these] <- lapply(part1[cast.these],
                castVariable, "categorical")
            var <- makeMR(pattern="mr_[123]", dataset=part1,
                name="test1", selections="1.0")
            test_that("set up MR for appending", {
                expect_true(is.Multiple(var))
            })
            with(test.dataset(mrdf), {
                part2 <- .setup
                part2[cast.these] <- lapply(part2[cast.these],
                    castVariable, "categorical")
                test_that("unbound subvariables get lined up", {
                    out <- try(appendDataset(part1, part2))
                    expect_false(is.error(out))
                    expect_true(is.dataset(out))
                    skip(expect_identical(length(batches(out)), 2L),
                        "3 != 2")
                    expect_identical(dim(out), c(nrow(mrdf)*2L, 2L))
                    expect_true(is.Multiple(out$test1))
                })
            })
        })
        
        with(test.dataset(mrdf[-3]), {
            part1 <- .setup
            cast.these <- grep("mr_", names(part1))
            part1[cast.these] <- lapply(part1[cast.these],
                castVariable, "categorical")
            var1 <- makeMR(pattern="mr_[12]", dataset=part1,
                name="test1", selections="1.0")
            with(test.dataset(mrdf[-1]), {
                part2 <- .setup
                part2[cast.these] <- lapply(part2[cast.these],
                    castVariable, "categorical")
                var2 <- makeMR(pattern="mr_[23]", dataset=part2,
                    name="test1", selections="1.0")
                test_that("set up MR for appending", {
                    expect_true(is.Multiple(var1))
                    expect_identical(names(subvariables(var1)),
                        c("mr_1", "mr_2"))
                    expect_true(is.Multiple(var2))
                    expect_identical(names(subvariables(var2)),
                        c("mr_2", "mr_3"))
                })
                test_that("arrays with different subvariables can append", {
                    out <- try(appendDataset(part1, part2))
                    expect_false(is.error(out))
                    expect_true(is.dataset(out))
                    skip(expect_identical(length(batches(out)), 2L),
                        "3 != 2")
                    expect_identical(dim(out), c(nrow(mrdf)*2L, 2L))
                    expect_true(is.Multiple(out$test1))
                    expect_identical(names(subvariables(out$test1)),
                        c("mr_1", "mr_2", "mr_3"))
                })
            })
        })
    })
}
