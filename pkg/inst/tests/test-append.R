context("Append datasets")

skip({

if (!run.only.local.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            part1 <- .setup
            cats <- categories(part1$v4)
            with(test.dataset(df), {
                part2 <- .setup
                test_that("append handles two identical Datasets", {
                    try(out <- appendDataset(part1, part2))
                    expect_false(is.error(out))
                    expect_true(is.dataset(out))
                    expect_identical(self(out), self(part1))
                    expect_identical(dim(out), c(nrow(df)*2L, ncol(df)))
                    expect_identical(categories(out$v4), cats)
                    expect_identical(as.vector(out$v3), rep(df$v3, 2))
                })
            })
        })
        
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
                    expect_identical(length(GET(part1@catalogs$batches)), 1L)
                    try(out <- appendDataset(part1, part2))
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
                    expect_identical(length(GET(part1@catalogs$batches)), 1L)
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
                    try(out <- appendDataset(part1, part2))
                    expect_false(is.error(out))
                    expect_true(is.dataset(out))
                    expect_identical(dim(out), c(nrow(mrdf)*2L, 2L))
                    expect_true(is.Multiple(out$test1))
                })
            })
        })
    })
}

})