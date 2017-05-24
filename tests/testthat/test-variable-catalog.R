context("Variable catalog")

with_mock_HTTP({
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
        expect_true(all(grepl("https://app.crunch.io/api/datasets/1/variables",
            urls(varcat))))
        expect_identical(self(varcat), "https://app.crunch.io/api/datasets/1/variables/")
        expect_identical(entities(ordering(varcat)), entities(varorder))
    })

    test_that("active/hidden getters", {
        expect_identical(index(active(varcat)),
            index(varcat)[urls(ordering(varcat))])
        expect_equivalent(index(hidden(varcat)), list())
        index(varcat)[[1]]$discarded <- TRUE
        expect_is(active(varcat), "VariableCatalog")
        expect_is(hidden(varcat), "VariableCatalog")
        expect_identical(urls(active(varcat)),
            c("https://app.crunch.io/api/datasets/1/variables/gender/",
            "https://app.crunch.io/api/datasets/1/variables/mymrset/",
            "https://app.crunch.io/api/datasets/1/variables/textVar/",
            "https://app.crunch.io/api/datasets/1/variables/starttime/",
            "https://app.crunch.io/api/datasets/1/variables/catarray/"))
        expect_length(active(varcat), 5)
        expect_identical(urls(hidden(varcat)),
            "https://app.crunch.io/api/datasets/1/variables/birthyr/")
        expect_length(hidden(varcat), 1)
        expect_length(varcat, 6)
        expect_identical(active(hidden(varcat)), hidden(active(varcat)))
    })

    gender.url <- "https://app.crunch.io/api/datasets/1/variables/gender/"
    test_that("Extract methods: character and numeric", {
        expect_is(varcat[[gender.url]], "VariableTuple")
        expect_identical(varcat[[gender.url]]@body,
            index(varcat)[[gender.url]])
        expect_identical(index(varcat[2:3]), index(varcat)[2:3])
    })

    test_that("Extract methods: invalid input", {
        expect_error(varcat[[999]], "subscript out of bounds") ## base R
        expect_null(varcat[["asdf"]])
        expect_null(varcat[[NA]])
        expect_error(varcat[999:1000], "Subscript out of bounds: 999:1000")
    })

    test_that("Extract methods: VariableOrder/Group", {
        ents <- c("https://app.crunch.io/api/datasets/1/variables/gender/",
            "https://app.crunch.io/api/datasets/1/variables/mymrset/")
        ord <- VariableOrder(VariableGroup("G1", entities=ents))
        expect_identical(names(varcat[ents]), c("Gender", "mymrset"))
        expect_identical(varcat[ord[[1]]], varcat[ents])
        expect_identical(varcat[ord], varcat[ents])
    })

    test_that("Construct Variable from Tuple", {
        expect_true(is.Categorical(CrunchVariable(varcat[[gender.url]])))
    })

    test_that("attribute getters", {
        expect_identical(names(varcat)[1:3],
            c("Birth Year", "Gender", "mymrset"))
        expect_identical(aliases(varcat)[1:2], c("birthyr", "gender"))
        expect_identical(types(varcat)[1:3],
            c("numeric", "categorical", "multiple_response"))
        expect_identical(descriptions(varcat[1:3]),
            c(NA, "Gender", "Please select all that apply"))
        expect_identical(notes(varcat[1:3]),
            c("Asked instead of age", "", ""))
    })

    test_that("attribute setters", {
        expect_PATCH(names(varcat)[1:3] <- c("Year of birth", "Gender", "Start time"),
            "https://app.crunch.io/api/datasets/1/variables/",
            '{"https://app.crunch.io/api/datasets/1/variables/birthyr/":{"name":"Year of birth"},',
            '"https://app.crunch.io/api/datasets/1/variables/mymrset/":{"name":"Start time"}}')
        expect_PATCH(notes(varcat)[1:3] <- c("Asked instead of age", "", "ms"),
            "https://app.crunch.io/api/datasets/1/variables/",
            '{"https://app.crunch.io/api/datasets/1/variables/mymrset/":{"notes":"ms"}}')
    })

    test_that("show method", {
        expect_output(varcat[1:3],
            get_output(data.frame(
                alias=c("birthyr", "gender", "mymrset"),
                name=c("Birth Year", "Gender", "mymrset"),
                type=c("numeric", "categorical", "multiple_response")
            )))
    })
})

with_test_authentication({
    ds <- newDataset(df)
    test_that("Can set descriptions (and doing so doesn't PUT order)", {
        with(temp.options(httpcache.log=""), {
            expect_identical(descriptions(variables(ds)),
                rep("", ncol(ds)))
            logs <- capture.output(descriptions(variables(ds))[2:3] <- c("Des 1", "Des 2"))
            expect_identical(descriptions(variables(ds))[1:4],
                c("", "Des 1", "Des 2", ""))
        })
        expect_length(logs, 2) ## PATCH, DROP
    })
    test_that("Get/set notes", {
        expect_identical(notes(variables(ds)), rep("", ncol(ds)))
        notes(variables(ds))[c(3,4)] <- c("The third variable", "Fourth")
        expect_identical(notes(variables(ds)),
            c("", "", "The third variable", "Fourth", "", ""))
        expect_identical(notes(variables(refresh(ds))),
            c("", "", "The third variable", "Fourth", "", ""))
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
        n3[c(2,4)] <- c("due", "quattro")
        aliases(variables(ds))[c(2,4)] <- c("due", "quattro")
        expect_identical(aliases(variables(ds)), n3)
        expect_identical(aliases(variables(refresh(ds))), n3)
    })

    test_that("Can [<- with VariableGroup/Order", {
        names(variables(ds))[2:3] <- c("two", "three")
        ord <- VariableOrder(VariableGroup("a group", entities=ds[2:3]))
        expect_identical(names(variables(ds)[ord]),
            c("two", "three"))
        try(names(variables(ds)[ord[[1]]]) <- c("TWO", "Three"))
        expect_identical(names(variables(ds)[ord]),
            c("TWO", "Three"))
        try(names(variables(ds)[ord]) <- c("2", "3"))
        expect_identical(names(variables(ds)[ord]),
            c("2", "3"))
    })
})
