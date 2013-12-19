context("Categorical Matrix")

if (!run.only.local.tests) {
    with(test.authentication, {
        test_that("can make Matrix in several ways", {
            with(test.dataset(mrdf), {
                testdf <- .setup
                var <- makeMatrix(testdf[1:3], name="test1")
                expect_true(is.Matrix(var))
                testdf <- refresh(testdf)
                expect_equal(c("test1", "v4"), names(testdf))
                name(var) <- "TESTONE"
                testdf <- refresh(testdf)
                expect_equal(c("TESTONE", "v4"), 
                    vapply(testdf, name, character(1), USE.NAMES=FALSE))
                    ## because names() point to variable aliases
            })
            with(test.dataset(mrdf), {
                testdf <- .setup
                var <- makeMatrix(testdf[c("mr_1", "mr_2", "mr_3")],
                    name="test1")
                expect_true(is.Matrix(var))
                testdf <- refresh(testdf)
                expect_equal(c("test1", "v4"), names(testdf))                
            })
            with(test.dataset(mrdf), {
                testdf <- .setup
                var <- makeMatrix(pattern="mr_[123]", dataset=testdf,
                    name="test1")
                expect_true(is.Matrix(var))
                testdf <- refresh(testdf)
                expect_equal(c("test1", "v4"), names(testdf))
            })
        })
        
        with(test.dataset(mrdf), {
            testdf <- .setup
            test_that("makeMatrix error conditions", {
                no.name <- "Must provide the name for the new variable"
                no.match <- "Pattern did not match any variables"
                need.variables <- "Invalid list of Variables to combine"
                ds.mismatch <- "`list_of_variables` must be from `dataset`"
                expect_error(makeMatrix(), no.name)
                expect_error(makeMatrix(pattern="mr_[123]", dataset=testdf),
                    no.name)
                expect_error(makeMatrix(pattern="rm_", dataset=testdf,
                    name="foo"), no.match)
                expect_error(makeMatrix(c("mr_1", "mr_2", "mr_3"),
                    dataset=testdf, name="foo"), need.variables)
                skip(with(test.dataset(df), {
                    nottestdf <- .setup
                    expect_error(makeMatrix(testdf[1:3], dataset=nottestdf,
                        name="test1"), ds.mismatch)
                }))
            })
        })
        
        test_that("can make MultipleResponse from Matrix", {
            with(test.dataset(mrdf), {
                testdf <- .setup
                var <- makeMatrix(pattern="mr_[123]", dataset=testdf,
                    name="test1")
                expect_true(is.Matrix(var))
                expect_true(is.categories(categories(var)))
                
                categories(var)[[1]]$selected <- TRUE
                var <- refresh(var)
                expect_true(is.Multiple(var))
                categories(var)[[1]]$selected <- FALSE
                var <- refresh(var)
                expect_true(is.Matrix(var))
                
                categories(var) <- dichotomize(categories(var), 1)
                var <- refresh(var)
                expect_true(is.Multiple(var))
                categories(var) <- undichotomize(categories(var))
                var <- refresh(var)
                expect_true(is.Matrix(var))
                
                var <- dichotomize(var, 1)
                expect_true(is.Multiple(var))
                var <- undichotomize(var)
                expect_true(is.Matrix(var))
            })
        })
    })
}

## then add tests for matrix variable entities. but note that subvariable names aren't exposed in API yet.