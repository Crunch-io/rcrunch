context("Categorical Array")

if (run.integration.tests) {
    with(test.authentication, {
        test_that("can make Categorical Array with Dataset subset", {
            with(test.dataset(mrdf), {
                var <- makeArray(ds[1:3], name="arrayVar")
                expect_true(is.CA(var))
                expect_false(any(unlist(lapply(tuple(var)$subvariables,
                    is.null)))) ## API sanity check
                ds <- refresh(ds)
                expect_equal(c("arrayVar", "v4"), names(ds))
                name(var) <- "TESTONE"
                ds <- refresh(ds)
                expect_equal(c("TESTONE", "v4"), names(variables(ds)))
            })
            with(test.dataset(mrdf), {
                var <- makeArray(ds[c("mr_1", "mr_2", "mr_3")],
                    name="arrayVar")
                expect_true(is.CA(var))
                ds <- refresh(ds)
                expect_equal(c("arrayVar", "v4"), names(ds))
                ## delete array variable
                u <- try(delete(ds$arrayVar))
                expect_that(u, is_not_an_error())
                ds <- refresh(ds)
                expect_identical(names(ds), "v4")
                expect_identical(ncol(ds), 1L)   
            })
        })
        test_that("can make Categorical Array with pattern", {
            with(test.dataset(mrdf), {
                var <- makeArray(pattern="mr_[123]", dataset=ds,
                    name="arrayVar")
                expect_true(is.CA(var))
                ds <- refresh(ds)
                expect_equal(c("arrayVar", "v4"), names(ds))
                ## unbind.
                u <- try(unbind(ds$arrayVar))
                expect_that(u, is_not_an_error())
                ds <- refresh(ds)
                expect_true(setequal(names(ds), names(mrdf)))
                expect_identical(ncol(ds), 4L)
            })
        })
        
        with(test.dataset(mrdf), {
            test_that("makeArray error conditions", {
                no.name <- "Must provide the name for the new variable"
                no.match <- "Pattern did not match any variables"
                ds.mismatch <- "`list_of_variables` must be from `dataset`"
                expect_error(makeArray(), no.name)
                expect_error(makeArray(pattern="mr_[123]", dataset=ds),
                    no.name)
                expect_error(makeArray(pattern="rm_", dataset=ds,
                    name="foo"), no.match)
                expect_true(is.CA(makeArray(c("mr_1", "mr_2", "mr_3"),
                    dataset=ds, name="foo")))
                skip("Errors, but with wrong error condition")
                with(test.dataset(df, "notds"), {
                    expect_error(makeArray(ds[1:3], dataset=notds,
                        name="arrayVar"), ds.mismatch)
                })
            })
        })
        
        test_that("can make MultipleResponse from CategoricalArray", {
            with(test.dataset(mrdf), {
                var <- makeArray(pattern="mr_[123]", dataset=ds,
                    name="arrayVar")
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
                cast.these <- grep("mr_", names(ds))
                ds[cast.these] <- lapply(ds[cast.these],
                    castVariable, "categorical")
                var <- makeMR(pattern="mr_[123]", dataset=ds,
                    name="arrayVar", selections="1.0")
                expect_true(is.Multiple(var))
                
                var <- undichotomize(var)
                expect_true(is.CA(var))
                
                ## unbind.
                u <- try(unbind(var))
                expect_that(u, is_not_an_error())
                ds <- refresh(ds)
                expect_true(setequal(names(ds), names(mrdf)))
                expect_identical(ncol(ds), 4L)
            })
            
            with(test.dataset(mrdf), {
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
                    expect_error(makeMR(pattern="mr_[123]", dataset=ds),
                        no.name)
                    expect_error(makeMR(pattern="rm_", dataset=ds,
                        name="foo", selections="foo"), no.match)
                    expect_error(makeMR(c("mr_1", "mr_2", "mr_3"),
                        dataset=ds, name="foo", selections="foo"),
                        not.categorical)
                    expect_error(makeMR(pattern="mr_[123]", dataset=ds,
                        name="arrayVar", selections="Not a Selection!"),
                        not.categorical)
                    cast.these <- grep("mr_", names(ds))
                    ds[cast.these] <- lapply(ds[cast.these],
                        castVariable, "categorical")
                    expect_error(makeMR(pattern="mr_[123]", dataset=ds,
                        name="arrayVar", selections="Not a Selection!"),
                        invalid.selection)
                    expect_error(makeMR(pattern="mr_[123]", dataset=ds,
                        name="arrayVar"), no.selections)
                    skip("Errors, but with Error in match(x, table, nomatch = 0L) : \n  'match' requires vector arguments\n")
                    with(test.dataset(df, "notds"), {
                        expect_error(makeMR(ds[1:3], dataset=notds,
                            name="arrayVar", selections="1.0"), ds.mismatch)
                    })
                })
            })
        })
    })
}
