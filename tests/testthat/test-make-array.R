context("Categorical Array")

with_mock_crunch({
    ds <- cachedLoadDataset("test ds")
    test_that("makeArray creates a VariableDefinition with dataset subset", {
        expect_json_equivalent(
            makeArray(ds[, "gender"], name = "Gender array"),
            list(
                name = "Gender array",
                subvariables = I("https://app.crunch.io/api/datasets/1/variables/gender/"),
                type = "categorical_array"
            )
        )
    })
    test_that("makeArray creates a VariableDefinition with list of variables", {
        expect_json_equivalent(
            makeArray(list(ds$gender), name = "Gender array"),
            list(
                name = "Gender array",
                subvariables = I("https://app.crunch.io/api/datasets/1/variables/gender/"),
                type = "categorical_array"
            )
        )
    })
    test_that("makeArray creates a VariableDefinition with variables subset", {
        expect_json_equivalent(
            makeArray(variables(ds)[names(ds) == "gender"],
                name = "Gender array 2"
            ),
            list(
                name = "Gender array 2",
                subvariables = I("https://app.crunch.io/api/datasets/1/variables/gender/"),
                type = "categorical_array"
            )
        )
    })
    test_that("makeMR creates a VariableDefinition", {
        expect_json_equivalent(
            makeMR(ds[, "gender"], name = "Gender MR", selections = "Male"),
            list(
                name = "Gender MR",
                subvariables = I("https://app.crunch.io/api/datasets/1/variables/gender/"),
                type = "multiple_response",
                selected_categories = I("Male")
            )
        )
    })

    no.name <- "Must provide the name for the new variable"
    no.match <- "No variables supplied"
    no.selections <- paste(
        "Must provide the names of the category or",
        "categories that indicate the dichotomous selection"
    )
    wrong.type <- "Expected subvariables to be either"
    invalid.selection <- "not found in variable's categories"
    not.categorical <- "are not Categorical"
    test_that("makeArray error conditions", {
        expect_error(makeArray(), no.name)
        expect_error(makeArray(ds[, "gender"]), no.name)
        expect_error(
            makeArray(ds[grep("NO variables", names(ds))], name = "foo"),
            no.match
        )
        expect_error(
            makeArray(ds$gender, name = "foo"),
            wrong.type
        )
    })
    test_that("makeMR error conditions", {
        expect_error(makeMR(), no.selections)
        expect_error(makeMR(ds[, "gender"]), no.selections)
        expect_error(makeMR(ds[, "gender"], selections = "Male"), no.name)
        expect_error(
            makeMR(ds[grep("NO variables", names(ds))],
                name = "foo",
                selections = "X"
            ),
            no.match
        )
        expect_error(
            makeMR(ds[, "gender"], selections = "Other", name = "Gen"),
            invalid.selection
        )
        expect_error(
            makeMR(ds[, c("gender", "birthyr")],
                selections = "Male",
                name = "Gen"
            ),
            not.categorical
        )
        expect_error(
            makeMR(ds[, c("gender", "NOTAVARIABLE")],
                selections = "Male",
                name = "Gen"
            ),
            "Undefined columns selected: NOTAVARIABLE"
        )
    })

    test_that("makeMRFromText errors correctly", {
        expect_error(
            makeMRFromText(ds$var, "; "),
            "Must supply a name for the new variable"
        )
        expect_error(
            makeMRFromText(ds$birthyr, name = "name"),
            paste0(
                dQuote("ds$birthyr"),
                " is of class NumericVariable, it must be a Crunch TextVariable."
            ),
            fixed = TRUE
        )
    })

    test_that("createSubvarDeriv generates the correct variable definition", {
        expected <- list(
            `function` = "case",
            args = list(
                list(
                    column = I(1:3),
                    type = list(
                        value = list(
                            class = "categorical",
                            categories = list(
                                list(
                                    id = 1,
                                    name = "No Data",
                                    numeric_value = NA,
                                    missing = TRUE
                                ),
                                list(
                                    id = 2,
                                    name = "Yes",
                                    numeric_value = NA,
                                    missing = FALSE
                                ),
                                list(
                                    id = 3,
                                    name = "No",
                                    numeric_value = NA,
                                    missing = FALSE
                                )
                            )
                        )
                    )
                ),
                list(
                    `function` = "is_missing",
                    args = list(
                        list(variable = "https://app.crunch.io/api/datasets/1/variables/textVar/")
                    )
                ),
                list(
                    `function` = "~=", # nolint
                    args = list(
                        list(variable = "https://app.crunch.io/api/datasets/1/variables/textVar/"),
                        list(value = "^oak; |; oak; |; oak$|^oak$")
                    )
                )
            ),
            references = list(name = "oak", alias = "textVar_oak")
        )
        varDef <- createSubvarDeriv(ds$textVar,
            str = "oak",
            delim = "; ",
            selected = "Yes",
            not_selected = "No",
            unanswered = NA
        )
        expect_equivalent(varDef, expected)
    })

    test_that("escapeRegex escapes all metacharacters", {
        metachars <- c(".", "^", "$", "*", "+", "?", "{", "}", "[", "]", "\\", "|", "(", ")")
        str <- paste0("vb", metachars, "net")
        expect_identical(escapeRegex(str), paste0("vb\\", metachars, "net"))
        expect_identical(escapeRegex("vb.a|net"), "vb\\.a\\|net")
    })

    test_that("buildDelimRegex generates the expected regular expression", {
        rx <- buildDelimRegex("maple", "; ")
        expect_true(grepl(rx, "maple"))
        expect_true(grepl(rx, "maple; birch"))
        expect_true(grepl(rx, "oak; maple; birch"))
        expect_true(grepl(rx, "birch; maple"))
        expect_false(grepl(rx, "birch; sugar maple"))
        expect_false(grepl(rx, "maple butter; oak"))
        # test delimiters that are regex characters
        expect_true(grepl(buildDelimRegex("maple", "| "), "oak| maple| birch"))
        expect_false(grepl(buildDelimRegex("maple", "| "), "oak| sugar maple| birch"))
    })

    test_that("makeMRFromText sends the correct variable derivation", {
        ds2 <- loadDataset("https://app.crunch.io/api/datasets/mr_from_delim/")
        trees <- c("birch", "sugar maple", "maple butter", "oak", "maple")
        expected <- VariableDefinition(
            derivation = zfunc(
                "select_categories", zfunc(
                    "array", zfunc(
                        "select",
                        list(map = lapply(trees, function(tree) {
                            return(createSubvarDeriv(ds2$delimed_text,
                                str = tree,
                                delim = "; ",
                                selected = "Yes",
                                not_selected = "No",
                                unanswered = NA
                            ))
                        })),
                        list(value = I(c(1, 2, 3, 4, 5)))
                    )
                ),
                list(value = I("selected"))
            ),
            name = "New Mr"
        )
        varDef <- makeMRFromText(ds2$delimed_text,
            delim = "; ",
            name = "New Mr",
            selected = "Yes",
            not_selected = "No",
            unanswered = NA
        )
        expect_equivalent(varDef, expected)
    })

    test_that("deriveArray with subvariable expr creates a categorical VariableDefinition", {
        expect_json_equivalent(
            deriveArray(
                subvariables = list(VariableDefinition(ds$gender == "Male", name = "male")),
                name = "Gender MR",
                numeric = FALSE
            ),
            list(
                name = "Gender MR",
                derivation = list(
                    `function` = "array",
                    args = list(list(
                        `function` = "make_frame",
                        args = list(list(
                            map = list(
                                c(zcl(ds$gender == "Male"), list(references = list(name = "male")))
                            )
                        ), list(value = I("0001")))
                    )),
                    kwargs = list(numeric = list(value = FALSE))
                )
            )
        )
    })

    test_that("deriveArray with subvariable expr creates a numeric VariableDefinition", {
        expect_json_equivalent(
            deriveArray(
                subvariables = list(VariableDefinition(ds$birthyr + 10, name = "birthyr + 10")),
                name = "birthyr addition",
                numeric = TRUE
            ),
            list(
                name = "birthyr addition",
                derivation = list(
                    `function` = "array",
                    args = list(list(
                        `function` = "make_frame",
                        args = list(list(
                            map = list(
                                c(zcl(ds$birthyr + 10), list(references = list(name = "birthyr + 10"))) #nolint
                            )
                        ), list(value = I("0001")))
                    )),
                    kwargs = list(numeric = list(value = TRUE))
                )
            )
        )
    })
})

with_test_authentication({
    whereas("We bind with makeArray", {
        ds <- mrdf.setup(newDataset(mrdf), name = "arrayVar")

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
        ds <- mrdf.setup(newDataset(mrdf), name = "arrayVar")
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

        test_that(paste0(
            "can make MultipleResponse from CategoricalArray by dichotomizing ",
            "categories (and back by undichotomize)"
        ), {
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
        ds$arrayVar <- makeMR(ds[cast.these], name = "arrayVar", selections = "1.0")
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
    whereas("makeMRFromText functions as expected", {
        ds <- mrdf.setup(newDataset(mrdf))
        v <- c("ma.ple; birch", "oak; ma.ple; birch", "birch; sugar maple", "maple butter; oak")
        ds$delim <- c(
            "ma.ple; birch", "oak; ma.ple; birch", "birch; sugar maple", "maple butter; oak"
        )
        test_that("makeMRFromText creates a variable", {
            ds$mr_5 <- makeMRFromText(ds$delim, delim = "; ", name = "myMR")
            expect_true(is.derived(ds$mr_5))
            expect_equivalent(dim(as.vector(ds$mr_5)), c(nrow(ds), 5))
        })
    })

    whereas("deriveArray with subvariables functions as expected", {
        ds <- newDataset(mrdf)
        ds$mrVar <- deriveArray(
            list(VariableDefinition(ds$v4 == "B", name = "subvar name", alias = "sv_alias")),
            name = "MR Variable",
            numeric = FALSE
        )

        test_that("can make MR with formMR", {
            expect_identical(names(ds), c(names(mrdf), "mrVar"))
            expect_true(is.MR(ds$mrVar))
            expect_true(is.Categorical(ds$mrVar$sv_alias))
            expect_equal(unclass(as.vector(table(ds$mrVar$sv_alias))), c(2, 2))
        })
    })
})
