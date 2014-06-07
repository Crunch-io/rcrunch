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
                message="Also no good",
                resolution="Also fixed"
            ), 
            list(
                message="Oh, and there was another problem",
                resolution="But it's also cool"
            )
        )
    )
    expect_identical(formatConflicts(c1), "No conflicts.")
    expect_identical(formatConflicts(c2), 
        "var1: Conflict: No good; Resolution: But I fixed it already")
    expect_identical(formatConflicts(c3), 
        c("var2: Conflict: No good; Resolution: But I fixed it already",
        "var1: (1) Conflict: Also no good; Resolution: Also fixed\n(2) Conflict: Oh, and there was another problem; Resolution: But it's also cool"))
    
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
                    out <- try(addBatchToDataset(part1, part2))
                    expect_true(is.character(out))
                    expect_true(grepl("/batches/", out))
                    expect_identical(length(batches(part1)), 2L)
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
                })
                test_that("append handles two identical Datasets", {
                    out <- try(appendDataset(part1, part2))
                    expect_false(is.error(out))
                    expect_true(is.dataset(out))
                    expect_identical(self(out), self(part1))
                    skip({
                        expect_identical(dim(out), c(nrow(df)*2L, ncol(df)))
                        expect_identical(getNrow(out), nrow(df)*2L)
                        expect_identical(nrow(out), length(as.vector(out$v3)))
                    }, "Dataset /summary/ isn't updating to the new row count")
                    expect_identical(categories(out$v4)[1:2], cats)
                    skip({
                        expect_equivalent(as.vector(out$v3), rep(df$v3, 2))
                        expect_identical(as.vector(out$v3), c(v3.1, v3.2))
                    }, "The second 20 rows are all NA")
                })
            })
        })


skip({
        
        with(test.dataset(df[,2:5]), {
            part1 <- .setup
            cats <- categories(part1$v4)
            with(test.dataset(df[1:3]), {
                part2 <- .setup
                test_that("append handles missing variables from each", {
                    p1.batches <- batches(part1)
                    expect_true(inherits(p1.batches, "ShojiCatalog"))
                    expect_identical(length(p1.batches), 1L)
                    expect_error(appendDataset(part1, part2, confirm=TRUE))
                    expect_identical(length(batches(part1)), 1L)
                    out <- try(appendDataset(part1, part2))
                    expect_false(is.error(out))
                    expect_true(is.dataset(out))
                    expect_identical(length(refresh(p1.batches)), 2L)
                    expect_identical(dim(out), c(nrow(df)*2L, 5L))
                    expect_identical(categories(out$v4), cats)
                    expect_identical(as.vector(out$v3), rep(df$v3, 2))
                    expect_identical(as.vector(out$v1), 
                        c(rep(NA, nrow(df)), df$v1))
                    expect_identical(as.vector(out$v5), 
                        c(df$v5, rep(NA, nrow(df))))
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
                    expect_identical(dim(out), c(nrow(mrdf)*2L, 2L))
                    expect_true(is.Multiple(out$test1))
                })
            })
        })
})
    })
}
