context("Categorical Array")

with_mock_HTTP({
    ds <- loadDataset("test ds")
    test_that("makeArray creates a VariableDefinition with dataset subset", {
        expect_json_equivalent(makeArray(ds[,"gender"], name="Gender array"),
            list(
                name="Gender array",
                subvariables=I("/api/datasets/dataset1/variables/gender/"),
                type="categorical_array"
            ))
    })
    test_that("makeArray creates a VariableDefinition with variables subset", {
        expect_json_equivalent(makeArray(variables(ds)[names(ds) == "gender"],
            name="Gender array 2"),
            list(
                name="Gender array 2",
                subvariables=I("/api/datasets/dataset1/variables/gender/"),
                type="categorical_array"
            ))
    })
    test_that("makeMR creates a VariableDefinition", {
        expect_json_equivalent(makeMR(ds[,"gender"], name="Gender MR", selections="Male"),
            list(
                name="Gender MR",
                subvariables=I("/api/datasets/dataset1/variables/gender/"),
                type="multiple_response",
                selected_categories=I("Male")
            ))
    })

    no.name <- "Must provide the name for the new variable"
    no.match <- "No variables supplied"
    no.selections <- paste("Must provide the names of the category or",
        "categories that indicate the dichotomous selection")
    invalid.selection <- "not found in variable's categories"
    not.categorical <- "are not Categorical"
    test_that("makeArray error conditions", {
        expect_error(makeArray(), no.name)
        expect_error(makeArray(ds[,"gender"]), no.name)
        expect_warning(
            expect_error(makeArray(pattern="rm_", dataset=ds, name="foo"),
                'argument "subvariables" is missing, with no default'),
            "argument to makeArray is no longer supported")
        expect_error(makeArray(ds[grep("NO variables", names(ds))], name="foo"),
            no.match)
    })
    test_that("makeMR error conditions", {
        expect_error(makeMR(), no.selections)
        expect_error(makeMR(ds[,"gender"]), no.selections)
        expect_error(makeMR(ds[,"gender"], selections="Male"), no.name)
        expect_warning(
            expect_error(makeMR(pattern="rm_", dataset=ds, name="foo",
                selections="X"),
                'argument "subvariables" is missing, with no default'),
            "argument to makeArray is no longer supported")
        expect_error(makeMR(ds[grep("NO variables", names(ds))], name="foo",
            selections="X"),
            no.match)
        expect_error(makeMR(ds[,"gender"], selections="Other", name="Gen"),
            invalid.selection)
        expect_error(makeMR(ds[,c("gender", "birthyr")], selections="Male",
            name="Gen"),
            not.categorical)
        expect_error(makeMR(ds[,c("gender", "NOTAVARIABLE")], selections="Male",
            name="Gen"),
            "Undefined columns selected: NOTAVARIABLE")
    })
})

with_test_authentication({
    whereas("We bind with makeArray", {
        ds <- newDataset(mrdf)
        ds$arrayVar <- makeArray(ds[1:3], name="arrayVar")

        test_that("can make Categorical Array with Dataset subset", {
            expect_equal(c("arrayVar", "v4"), names(ds))
            expect_true(is.CA(ds$arrayVar))
        })
        test_that("can delete the array we just bound", {
            ds$arrayVar <- NULL
            expect_identical(names(ds), "v4")
            expect_identical(ncol(ds), 1L)
        })
    })

    whereas("Testing dichotomizing and undichotomizing", {
        ds <- newDataset(mrdf)
        ds$arrayVar <- makeArray(ds[c("mr_1", "mr_2", "mr_3")], name="arrayVar")
        var <- ds$arrayVar
        test_that("setup to make MultipleResponse from CategoricalArray", {
            expect_true(is.CA(var))
        })

        test_that("can make MultipleResponse from CategoricalArray by editing category$selected", {
            categories(var)[[1]]$selected <- TRUE
            var <- refresh(var) ## Refresh required if changing type by editing categories
            expect_true(is.Multiple(var))
            categories(var)[[1]]$selected <- FALSE
            var <- refresh(var) ## Refresh required if changing type by editing categories
            expect_true(is.CA(var))
        })

        test_that("can make MultipleResponse from CategoricalArray by dichotomizing categories (and back by undichotomize)", {
            categories(var) <- dichotomize(categories(var), 1)
            var <- refresh(var) ## Refresh required if changing type by editing categories
            expect_true(is.Multiple(var))
            categories(var) <- undichotomize(categories(var))
            var <- refresh(var) ## Refresh required if changing type by editing categories
            expect_true(is.CA(var))
        })
        test_that("can (un)dichotomize directly on the variable", {
            var <- dichotomize(var, 1)
            expect_true(is.Multiple(var))
            expect_true(is.Multiple(refresh(var)))
            var <- undichotomize(var)
            expect_true(is.CA(var))
            expect_true(is.CA(refresh(var)))
        })
        test_that("can (un)dichotomize on var in dataset", {
            ds <- refresh(ds)
            ds$arrayVar <- dichotomize(ds$arrayVar, 1)
            expect_true(is.Multiple(ds$arrayVar))
            expect_true(is.Multiple(refresh(ds)$arrayVar))
            ds$arrayVar <- undichotomize(ds$arrayVar)
            expect_true(is.CA(ds$arrayVar))
            expect_true(is.CA(refresh(ds)$arrayVar))
        })
    })

    test_that("can make MultipleResponse directly", {
        ds <- newDataset(mrdf)
        cast.these <- grep("mr_", names(ds))
        ds[cast.these] <- lapply(ds[cast.these], castVariable, "categorical")
        ds$arrayVar <- makeMR(ds[cast.these], name="arrayVar", selections="1.0")
        var <- ds$arrayVar
        expect_true(is.Multiple(var))

        var <- undichotomize(var)
        expect_true(is.CA(var))

        ## unbind.
        u <- unbind(var)
        ds <- refresh(ds)
        expect_true(setequal(names(ds), names(mrdf)))
        expect_identical(ncol(ds), 4L)
    })

    whereas("Making derived arrays", {
        ds <- newDatasetFromFixture("apidocs")
        vd <- deriveArray(list(ds$q1, ds$petloc$petloc_home), name="Derived pets")
        expect_is(vd, "VariableDefinition")
        ds$derivedarray <- vd
        expect_true(is.CA(ds$derivedarray))
    })
})
