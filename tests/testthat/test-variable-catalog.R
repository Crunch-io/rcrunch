context("Variable catalog")

with_mock_crunch({
    ds <- loadDataset("test ds")
    varcat <- allVariables(ds)
    varorder <- ordering(varcat)
    test_that("VariableCatalog instantiates from Shoji", {
        expect_is(varcat, "VariableCatalog")
    })

    test_that("VariableCatalog index method", {
        expect_identical(names(index(varcat)), names(varcat@index))
    })

    test_that("VariableCatalog has the right contents", {
        expect_true(all(grepl(
            "https://app.crunch.io/api/datasets/1/variables",
            urls(varcat)
        )))
        expect_identical(self(varcat), "https://app.crunch.io/api/datasets/1/variables/")
        expect_identical(entities(ordering(varcat)), entities(varorder))
    })

    test_that("hidden getters", {
        expect_is(hidden(varcat), "VariableFolder")
        expect_length(hidden(varcat), 0)
    })
    
    test_that("active variables don't include hidden (now a dataset property)", {
        expect_identical(
            index(variables(ds)),
            index(varcat)[urls(ordering(varcat))]
        )
        ds@hiddenVariables <- ds@variables[1]
        expect_is(variables(ds), "VariableCatalog")
        # Specific behavior of "hidden" is tested in test-hide-variables.R
        expect_identical(
            urls(variables(ds)),
            c(
                "https://app.crunch.io/api/datasets/1/variables/gender/",
                "https://app.crunch.io/api/datasets/1/variables/location/",
                "https://app.crunch.io/api/datasets/1/variables/mymrset/",
                "https://app.crunch.io/api/datasets/1/variables/textVar/",
                "https://app.crunch.io/api/datasets/1/variables/starttime/",
                "https://app.crunch.io/api/datasets/1/variables/catarray/"
            )
        )
        expect_length(variables(ds), 6)
    })

    test_that("secure variables aren't considered active (#383)", {
        index(ds@variables)[[1]]$secure <- TRUE

        expect_identical(
            urls(variables(ds)),
            c(
                "https://app.crunch.io/api/datasets/1/variables/gender/",
                "https://app.crunch.io/api/datasets/1/variables/location/",
                "https://app.crunch.io/api/datasets/1/variables/mymrset/",
                "https://app.crunch.io/api/datasets/1/variables/textVar/",
                "https://app.crunch.io/api/datasets/1/variables/starttime/",
                "https://app.crunch.io/api/datasets/1/variables/catarray/"
            )
        )
        expect_length(variables(ds), 6)
    })

    gender.url <- "https://app.crunch.io/api/datasets/1/variables/gender/"
    test_that("Extract methods: character and numeric", {
        expect_is(varcat[[gender.url]], "VariableTuple")
        expect_identical(
            varcat[[gender.url]]@body,
            index(varcat)[[gender.url]]
        )
        expect_identical(index(varcat[2:3]), index(varcat)[2:3])
    })

    test_that("Extract methods: invalid input", {
        expect_error(varcat[[999]], "subscript out of bounds") ## base R
        expect_null(varcat[["asdf"]])
        expect_null(varcat[[NA]])
        expect_error(varcat[999:1000], "Subscript out of bounds: 999:1000")
    })

    test_that("Extract methods: VariableOrder/Group", {
        ents <- c(
            "https://app.crunch.io/api/datasets/1/variables/gender/",
            "https://app.crunch.io/api/datasets/1/variables/mymrset/"
        )
        ord <- VariableOrder(VariableGroup("G1", entities = ents))
        expect_identical(names(varcat[ents]), c("Gender", "mymrset"))
        expect_identical(varcat[ord[[1]]], varcat[ents])
        expect_identical(varcat[ord], varcat[ents])
    })

    test_that("Construct Variable from Tuple", {
        expect_true(is.Categorical(CrunchVariable(varcat[[gender.url]])))
    })

    test_that("attribute getters", {
        expect_identical(
            names(varcat)[1:4],
            c("Birth Year", "Gender", "Categorical Location", "mymrset")
        )
        expect_identical(aliases(varcat)[1:2], c("birthyr", "gender"))
        expect_identical(
            types(varcat)[1:4],
            c("numeric", "categorical", "categorical", "multiple_response")
        )
        expect_identical(
            descriptions(varcat[1:4]),
            c("", "Gender", "Location test", "Please select all that apply")
        )
        expect_identical(
            notes(varcat[1:4]),
            c("Asked instead of age", "", "", "")
        )
    })

    test_that("attribute setters", {
        expect_PATCH(
            names(varcat)[1:4] <- c("Year of birth", "Gender", "Loc", "Start time"),
            "https://app.crunch.io/api/datasets/1/variables/",
            '{"element":"shoji:catalog","index":{"https://app.crunch.io/api/',
            'datasets/1/variables/birthyr/":{"name":"Year of birth"},',
            '"https://app.crunch.io/api/datasets/1/variables/location/":{"name":"Loc"},',
            '"https://app.crunch.io/api/datasets/1/variables/mymrset/":{"name":"Start time"}}}'
        )
        expect_PATCH(
            notes(varcat)[1:4] <- c("Asked instead of age", "", "", "ms"),
            "https://app.crunch.io/api/datasets/1/variables/",
            '{"element":"shoji:catalog","index":{"https://app.crunch.io/api/',
            'datasets/1/variables/mymrset/":{"notes":"ms"}}}'
        )
    })
    test_that("attribute setters with duplication", {
        ## In the first case, the first element is duplicated, but it's getting
        ## the same value, so we can ignore it
        names <- c("Year of birth", "Year of birth", "Gender", "Loc", "Start time")
        expect_PATCH(
            names(varcat[c(1, 1:4)]) <- names,
            "https://app.crunch.io/api/datasets/1/variables/",
            '{"element":"shoji:catalog","index":{"https://app.crunch.io/api/',
            'datasets/1/variables/birthyr/":{"name":"Year of birth"},',
            '"https://app.crunch.io/api/datasets/1/variables/location/":{"name":"Loc"},',
            '"https://app.crunch.io/api/datasets/1/variables/mymrset/":{"name":"Start time"}}}'
        )
        ## In the second case, the first element of the catalog is getting
        ## assigned two different new values, so that errors.
        expect_error(
            names(varcat[c(1, 1:4)]) <- c("Year of birth", "BY", "Gender", "Loc", "Start time"),
            "Can't update the same index item with more than one entry"
        )
    })

    test_that("show method", {
        expect_prints(
            varcat[1:4],
            get_output(data.frame(
                alias = c("birthyr", "gender", "location", "mymrset"),
                name = c("Birth Year", "Gender", "Categorical Location", "mymrset"),
                type = c("numeric", "categorical", "categorical", "multiple_response")
            ))
        )
    })

    test_that("VariableCatalog as.data.frame method", {
        expect_identical(
            as.data.frame(varcat[1:3]),
            data.frame(
                alias = c("birthyr", "gender", "location"),
                name = c("Birth Year", "Gender", "Categorical Location"),
                type = c("numeric", "categorical", "categorical"),
                stringsAsFactors = FALSE
            )
        )
        expect_identical(
            as.data.frame(varcat[1:3], row.names = urls(varcat[1:3])),
            data.frame(
                alias = c("birthyr", "gender", "location"),
                name = c("Birth Year", "Gender", "Categorical Location"),
                type = c("numeric", "categorical", "categorical"),
                row.names = urls(varcat[1:3]),
                stringsAsFactors = FALSE
            )
        )
    })
    test_that("as.data.frame method returns all fields when keys = TRUE", {
        varDF <- as.data.frame(varcat, keys = TRUE)
        expect_identical(
            names(varDF),
            c(
                "name", "discarded", "alias", "type", "id", "description",
                "notes", "uniform_basis", "subvariables", "subvariables_catalog", "resolution",
                "rollup_resolution"
            )
        )
        expect_identical(
            varDF$subvariables[c(3, 4, 7)],
            list(
                NA,
                list(
                    "mymrset/subvariables/subvar2/",
                    "mymrset/subvariables/subvar1/",
                    "mymrset/subvariables/subvar3/"
                ),
                list(
                    "mymrset/subvariables/subvar2/",
                    "mymrset/subvariables/subvar1/",
                    "mymrset/subvariables/subvar3/"
                )
            )
        )
    })
    test_that("list columns are homogeneous type", {
        skip("TODO: ensure something about the elements of a 'list column'")
        vc2 <- varcat
        vc2@index[[3]]$subvariables <- "just/one/subvar"
        vc2DF <- as.data.frame(vc2, keys = "all")
        expect_identical(
            vc2DF$subvariables[c(3, 4, 7)],
            list(
                list("just/one/subvar"),
                list(
                    "mymrset/subvariables/subvar2/",
                    "mymrset/subvariables/subvar1/",
                    "mymrset/subvariables/subvar3/"
                ),
                list(
                    "mymrset/subvariables/subvar2/",
                    "mymrset/subvariables/subvar1/",
                    "mymrset/subvariables/subvar3/"
                )
            )
        )
    })

    test_that("As.data.frame method errors correctly", {
        expect_error(
            as.data.frame(varcat[1:3], keys = "Not a field at all"),
            paste(
                dQuote("Not a field at all"),
                "is an invalid key for catalogs of class VariableCatalog."
            )
        )
        expect_error(
            as.data.frame(varcat[1:3], keys = c("banana", "fooey")),
            paste(
                serialPaste(dQuote(c("banana", "fooey"))),
                "are invalid keys for catalogs of class VariableCatalog."
            )
        )
        expect_error(
            as.data.frame(varcat[1:3], keys = c("name", "fooey")),
            paste(dQuote("fooey"), "is an invalid key for catalogs of class VariableCatalog.")
        )
    })
})

with_test_authentication({
    ds <- newDataset(df)
    test_that("Can set descriptions (and doing so doesn't PUT order)", {
        with(temp.options(httpcache.log = ""), {
            expect_identical(
                descriptions(variables(ds)),
                rep("", ncol(ds))
            )
            logs <- capture.output(descriptions(variables(ds))[2:3] <- c("Des 1", "Des 2"))
            expect_identical(
                descriptions(variables(ds))[1:4],
                c("", "Des 1", "Des 2", "")
            )
        })
        expect_length(logs, 2) ## PATCH, DROP
    })
    test_that("Get/set notes", {
        expect_identical(notes(variables(ds)), rep("", ncol(ds)))
        notes(variables(ds))[c(3, 4)] <- c("The third variable", "Fourth")
        expect_identical(
            notes(variables(ds)),
            c("", "", "The third variable", "Fourth", "", "")
        )
        expect_identical(
            notes(variables(refresh(ds))),
            c("", "", "The third variable", "Fourth", "", "")
        )
    })
    test_that("Can set names and aliases", {
        n <- names(df)
        expect_identical(names(variables(ds)), n)
        expect_identical(aliases(variables(ds)), n)
        names(variables(ds))[2:3] <- c("two", "three")
        n2 <- n
        n2[2:3] <- c("two", "three")
        expect_identical(names(variables(ds)), n2)
        expect_identical(names(variables(refresh(ds))), n2)
        n3 <- n
        n3[c(2, 4)] <- c("due", "quattro")
        aliases(variables(ds))[c(2, 4)] <- c("due", "quattro")
        expect_identical(aliases(variables(ds)), n3)
        expect_identical(aliases(variables(refresh(ds))), n3)
    })

    test_that("Can [<- with VariableGroup/Order", {
        names(variables(ds))[2:3] <- c("two", "three")
        ord <- VariableOrder(VariableGroup("a group", entities = ds[2:3]))
        expect_identical(
            names(variables(ds)[ord]),
            c("two", "three")
        )
        try(names(variables(ds)[ord[[1]]]) <- c("TWO", "Three"))
        expect_identical(
            names(variables(ds)[ord]),
            c("TWO", "Three")
        )
        try(names(variables(ds)[ord]) <- c("2", "3"))
        expect_identical(
            names(variables(ds)[ord]),
            c("2", "3")
        )
    })
})
