context("Categorical Array")

if (!run.only.local.tests) {
    with(test.authentication, {
        test_that("can make Categorical Array in several ways", {
            with(test.dataset(mrdf), {
                testdf <- .setup
                var <- makeArray(testdf[1:3], name="test1")
                expect_true(is.CA(var))
                testdf <- refresh(testdf)
                expect_equal(c("test1", "v4"), names(testdf))
                skip({
                    name(var) <- "TESTONE"
                    testdf <- refresh(testdf)
                    expect_equal(c("TESTONE", "v4"), variableNames(testdf))
                        ## because names() point to variable aliases
                }, "investigate backend error")
            })
            with(test.dataset(mrdf), {
                testdf <- .setup
                var <- makeArray(testdf[c("mr_1", "mr_2", "mr_3")],
                    name="test1")
                expect_true(is.CA(var))
                testdf <- refresh(testdf)
                expect_equal(c("test1", "v4"), names(testdf))
                ## delete array variable
                u <- try(delete(testdf$test1))
                expect_false(is.error(u))
                testdf <- refresh(testdf)
                expect_identical(names(testdf), "v4")
                expect_identical(ncol(testdf), 1L)   
            })
            with(test.dataset(mrdf), {
                testdf <- .setup
                var <- makeArray(pattern="mr_[123]", dataset=testdf,
                    name="test1")
                expect_true(is.CA(var))
                testdf <- refresh(testdf)
                expect_equal(c("test1", "v4"), names(testdf))
                ## unbind.
                u <- try(unbind(testdf$test1))
                expect_false(is.error(u))
                testdf <- refresh(testdf)
                expect_true(setequal(names(testdf), names(mrdf)))
                expect_identical(ncol(testdf), 4L)
            })
        })
        
        with(test.dataset(mrdf), {
            testdf <- .setup
            test_that("makeArray error conditions", {
                no.name <- "Must provide the name for the new variable"
                no.match <- "Pattern did not match any variables"
                ds.mismatch <- "`list_of_variables` must be from `dataset`"
                expect_error(makeArray(), no.name)
                expect_error(makeArray(pattern="mr_[123]", dataset=testdf),
                    no.name)
                expect_error(makeArray(pattern="rm_", dataset=testdf,
                    name="foo"), no.match)
                expect_true(is.CA(makeArray(c("mr_1", "mr_2", "mr_3"),
                    dataset=testdf, name="foo")))
                skip(with(test.dataset(df), {
                    nottestdf <- .setup
                    expect_error(makeArray(testdf[1:3], dataset=nottestdf,
                        name="test1"), ds.mismatch)
                }))
            })
        })
        
        test_that("can make MultipleResponse from CategoricalArray", {
            with(test.dataset(mrdf), {
                testdf <- .setup
                var <- makeArray(pattern="mr_[123]", dataset=testdf,
                    name="test1")
                expect_true(is.CA(var))
                expect_true(is.categories(categories(var)))
                
                categories(var)[[1]]$selected <- TRUE
                var <- refresh(var)
                expect_true(is.Multiple(var))
                categories(var)[[1]]$selected <- FALSE
                var <- refresh(var)
                expect_true(is.CA(var))
                
                categories(var) <- dichotomize(categories(var), 1)
                var <- refresh(var)
                expect_true(is.Multiple(var))
                categories(var) <- undichotomize(categories(var))
                var <- refresh(var)
                expect_true(is.CA(var))
                
                var <- dichotomize(var, 1)
                expect_true(is.Multiple(var))
                var <- undichotomize(var)
                expect_true(is.CA(var))
            })
        })
        
        test_that("can make MultipleResponse directly", {
            with(test.dataset(mrdf), {
                testdf <- .setup
                cast.these <- grep("mr_", names(testdf))
                testdf[cast.these] <- lapply(testdf[cast.these],
                    castVariable, "categorical")
                var <- makeMR(pattern="mr_[123]", dataset=testdf,
                    name="test1", selections="1.0")
                expect_true(is.Multiple(var))
                
                var <- undichotomize(var)
                expect_true(is.CA(var))
                
                ## unbind.
                u <- try(unbind(var))
                expect_false(is.error(u))
                testdf <- refresh(testdf)
                expect_true(setequal(names(testdf), names(mrdf)))
                expect_identical(ncol(testdf), 4L)
            })
            
            with(test.dataset(mrdf), {
                testdf <- .setup
                test_that("makeMR error conditions", {
                    no.name <- "Must provide the name for the new variable"
                    no.match <- "Pattern did not match any variables"
                    need.variables <- "Invalid list of Variables to combine"
                    ds.mismatch <- "`list_of_variables` must be from `dataset`"
                    no.selections <- paste("Must provide the names of the", 
                        "category or categories that indicate the dichotomous",
                        "selection")
                    invalid.selection <- paste("not found in",
                        "variable's categories")
                    not.categorical <- "are not Categorical"
                    expect_error(makeMR(), no.name)
                    expect_error(makeMR(pattern="mr_[123]", dataset=testdf),
                        no.name)
                    expect_error(makeMR(pattern="rm_", dataset=testdf,
                        name="foo", selections="foo"), no.match)
                    expect_error(makeMR(c("mr_1", "mr_2", "mr_3"),
                        dataset=testdf, name="foo", selections="foo"),
                        not.categorical)
                    expect_error(makeMR(pattern="mr_[123]", dataset=testdf,
                        name="test1", selections="Not a Selection!"),
                        not.categorical)
                    skip(with(test.dataset(df), {
                        nottestdf <- .setup
                        expect_error(makeMR(testdf[1:3], dataset=nottestdf,
                            name="test1"), ds.mismatch)
                    }), "userdataset problem?")
                    cast.these <- grep("mr_", names(testdf))
                    testdf[cast.these] <- lapply(testdf[cast.these],
                        castVariable, "categorical")
                    expect_error(makeMR(pattern="mr_[123]", dataset=testdf,
                        name="test1", selections="Not a Selection!"),
                        invalid.selection)
                    expect_error(makeMR(pattern="mr_[123]", dataset=testdf,
                        name="test1"), no.selections)
                })
            })
        })
    })
}

## then add tests for matrix variable entities. but note that subvariable names aren't exposed in API yet.