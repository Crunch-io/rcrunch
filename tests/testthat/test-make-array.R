context("Categorical Array")

with_mock_crunch({
    ds <- loadDataset("test ds")
    test_that("makeArray creates a VariableDefinition with dataset subset", {
        expect_json_equivalent(makeArray(ds[,"gender"], name="Gender array"),
            list(
                name="Gender array",
                subvariables=I("https://app.crunch.io/api/datasets/1/variables/gender/"),
                type="categorical_array"
            ))
    })
    test_that("makeArray creates a VariableDefinition with variables subset", {
        expect_json_equivalent(makeArray(variables(ds)[names(ds) == "gender"],
            name="Gender array 2"),
            list(
                name="Gender array 2",
                subvariables=I("https://app.crunch.io/api/datasets/1/variables/gender/"),
                type="categorical_array"
            ))
    })
    test_that("makeMR creates a VariableDefinition", {
        expect_json_equivalent(makeMR(ds[,"gender"], name="Gender MR", selections="Male"),
            list(
                name="Gender MR",
                subvariables=I("https://app.crunch.io/api/datasets/1/variables/gender/"),
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
        expect_error(makeArray(ds[grep("NO variables", names(ds))], name="foo"),
            no.match)
    })
    test_that("makeMR error conditions", {
        expect_error(makeMR(), no.selections)
        expect_error(makeMR(ds[,"gender"]), no.selections)
        expect_error(makeMR(ds[,"gender"], selections="Male"), no.name)
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

    test_that("mrFromDelim errors correctly", {
        expect_error(mrFromDelim(ds$var, "; ",),
            "Must supply a name for the new variable")
        expect_error(mrFromDelim(mtcars$cyl,  name = "name"),
            paste0(dQuote("var"), " must be a Categorical or Text Crunch Variable."))
    })

    test_that("createSubvarDef generates the correct variable definition", {
         v <- c("maple; birch", "oak; maple; birch", "birch; sugar maple", "maple butter; oak", NA)
         expected <- structure(list(
             values = c(-1L, -1L, -1L, -1L, -1L),
             type = "categorical",
             categories = list(
                 structure(list(
                     id = -1L,
                     name = "No Data",
                     numeric_value = NULL,
                     missing = TRUE
                     ),
                     .Names = c("id", "name", "numeric_value", "missing")
                     )
                 ),
             name = "oak"),
             .Names = c("values", "type", "categories", "name"),
             class = "VariableDefinition")
         varDef <- crunch:::createSubvarDef(v, str = "oak",
             delim = "; ",
             selected = "Yes",
             not_selected = "No",
             unanswered = v[is.na(v)])
         expect_equivalent(varDef, expected)
    })
    c("maple; birch", "oak; maple; birch", "birch; sugar maple", "maple butter; oak")
    test_that("buildDelimRegex generates the expected regular expression", {
        rx <- buildDelimRegex("maple", "; ")
        expect_true(grepl(rx, "maple"))
        expect_true(grepl(rx, "maple; birch"))
        expect_true(grepl(rx, "oak; maple; birch"))
        expect_true(grepl(rx, "birch; maple"))
        expect_false(grepl(rx, "birch; sugar maple"))
        expect_false(grepl(rx, "maple butter; oak"))
        #test delimiters that are regex characters
        expect_true(grepl(buildDelimRegex("maple", "| "), "oak| maple| birch"))
        expect_false(grepl(buildDelimRegex("maple", "| "), "oak| sugar maple| birch"))
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
            with_consent(ds$arrayVar <- NULL)
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
    whereas("mrFromDelim functions as expected", {
        ds <- newDataset(mrdf)
        v <- c("maple; birch", "oak; maple; birch", "birch; sugar maple", "maple butter; oak")
        ds$delim <- c("maple; birch", "oak; maple; birch", "birch; sugar maple", "maple butter; oak")
        test_that("mrFromDelim creates a variable", {
            ds$mr_5 <- mrFromDelim(ds$delim, delim = "; ", name = "myMR")
            expect_identical(names(subvariables(ds$mr_5)),
                c("maple", "birch", "oak", "sugar maple", "maple butter"))
            expect_identical(names(categories(ds$mr_5)),
                c("not_selected", "selected", "No Data"))
            expect_identical(as.vector(ds$mr_5$maple),
                structure(c(2L, 2L, 1L, 1L), .Label = c("not_selected", "selected"
                ), class = "factor"))
            expect_identical(hiddenVariables(ds), "delim")
        })
    })
})
