context("Append datasets")

skip({

if (!run.only.local.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            part1 <- .setup
            with(test.dataset(df), {
                part2 <- .setup
                test_that("append handles two identical Datasets", {
                    try(out <- appendDataset(part1, part2))
                    expect_false(is.error(out))
                    expect_identical(dim(out), c(nrow(df)*2L, ncol(df)))
                    expect_identical(categories(out$v4), categories(part1$v4))
                    expect_identical(as.vector(out$v3), rep(df$v3, 2))
                })
            })
        })
        
        with(test.dataset(df[,2:5]), {
            part1 <- .setup
            with(test.dataset(df[1:3]), {
                test_that("append handles missing variables from each", {
                    
                })
            })
        })
    })
}

})