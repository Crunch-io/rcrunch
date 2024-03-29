context("Add a variable to a dataset")

test_that("toVariable parses R numerics", {
    expect_identical(
        toVariable(2L:4L, name = "Numbers!", alias = "num"),
        structure(list(
            values = 2L:4L, type = "numeric", name = "Numbers!",
            alias = "num"
        ), class = "VariableDefinition")
    )
    expect_equivalent(
        toVariable(2L:4L, name = "Numbers!", alias = "num"),
        list(values = 2L:4L, type = "numeric", name = "Numbers!", alias = "num")
    )
})
test_that("toVariable parses R characters", {
    expect_identical(
        toVariable(letters[1:3]),
        structure(list(values = c("a", "b", "c"), type = "text"),
            class = "VariableDefinition"
        )
    )
})
test_that("toVariable parses factors", {
    expect_identical(
        toVariable(as.factor(c(rep(LETTERS[2:3], 3), NA))),
        VarDef(
            values = c(1L, 2L, 1L, 2L, 1L, 2L, -1L),
            type = "categorical",
            categories = list(
                list(id = 1L, name = "B", numeric_value = 1L, missing = FALSE),
                list(id = 2L, name = "C", numeric_value = 2L, missing = FALSE),
                list(id = -1L, name = "No Data", numeric_value = NULL, missing = TRUE)
            )
        )
    )
})

test_that("toVariable parses logical", {
    expect_equivalent(
        toVariable(c(TRUE, FALSE, FALSE, NA, TRUE)),
        VarDef(values = c(1L, 0L, 0L, -1L, 1L), type = "categorical", categories = list(
            list(id = 1L, name = "True", numeric_value = 1L, missing = FALSE, selected = TRUE),
            list(id = 0L, name = "False", numeric_value = 0L, missing = FALSE),
            list(id = -1L, name = "No Data", numeric_value = NULL, missing = TRUE)
        ))
    )
})
test_that("toVariable parses AsIses", {
    expect_identical(
        toVariable(I(1:5)),
        structure(list(values = 1L:5L, type = "numeric"),
            class = "VariableDefinition"
        )
    )
    expect_identical(
        toVariable(I(letters[1:3])),
        structure(list(values = c("a", "b", "c"), type = "text"),
            class = "VariableDefinition"
        )
    )
})

test_that("toVariable parses haven::labelled", {
    labelled <- haven::labelled(
        rep(LETTERS[1:3], 3),
        structure(LETTERS[1:3], names = LETTERS[1:3])
    )
    expect_equivalent(
        toVariable(labelled),
        list(values = rep(1:3, 3), type = "categorical", categories = list(
            list(id = 1L, name = "A", numeric_value = 1L, missing = FALSE),
            list(id = 2L, name = "B", numeric_value = 2L, missing = FALSE),
            list(id = 3L, name = "C", numeric_value = 3L, missing = FALSE),
            list(id = -1L, name = "No Data", numeric_value = NULL, missing = TRUE)
        ))
    )

    # backwards compatilbity with old class names
    class(labelled) <- "labelled"
    expect_equivalent(
        toVariable(labelled),
        list(values = rep(1:3, 3), type = "categorical", categories = list(
            list(id = 1L, name = "A", numeric_value = 1L, missing = FALSE),
            list(id = 2L, name = "B", numeric_value = 2L, missing = FALSE),
            list(id = 3L, name = "C", numeric_value = 3L, missing = FALSE),
            list(id = -1L, name = "No Data", numeric_value = NULL, missing = TRUE)
        ))
    )

    # even if only some values are labelled, the values are still used
    labelled <- haven::labelled(
        rep(LETTERS[1:3], 3),
        structure(LETTERS[2], names = LETTERS[2])
    )
    expect_equivalent(
        toVariable(labelled),
        list(values = rep(1:3, 3), type = "categorical", categories = list(
            list(id = 1L, name = "A", numeric_value = 1L, missing = FALSE),
            list(id = 2L, name = "B", numeric_value = 2L, missing = FALSE),
            list(id = 3L, name = "C", numeric_value = 3L, missing = FALSE),
            list(id = -1L, name = "No Data", numeric_value = NULL, missing = TRUE)
        ))
    )


    # If the values are numeric, we still get a categorical
    labelled <- haven::labelled(
        rep(1:3, 3),
        structure(2,
            names = LETTERS[2]
        )
    )
    expect_equivalent(
        toVariable(labelled),
        list(values = rep(1:3, 3), type = "categorical", categories = list(
            list(id = 1L, name = "1", numeric_value = 1L, missing = FALSE),
            list(id = 2L, name = "2", numeric_value = 2L, missing = FALSE),
            list(id = 3L, name = "3", numeric_value = 3L, missing = FALSE),
            list(id = -1L, name = "No Data", numeric_value = NULL, missing = TRUE)
        ))
    )
})

test_that("toVariable parses haven::labelled_spss", {
    labelled <- haven::labelled_spss(rep(LETTERS[1:3], 3),
        structure(LETTERS[2:3],
            names = LETTERS[2:3]
        ),
        na_values = LETTERS[1]
    )
    expect_equivalent(
        toVariable(labelled),
        list(values = rep(1:3, 3), type = "categorical", categories = list(
            list(id = 1L, name = "A", numeric_value = 1L, missing = TRUE),
            list(id = 2L, name = "B", numeric_value = 2L, missing = FALSE),
            list(id = 3L, name = "C", numeric_value = 3L, missing = FALSE),
            list(id = -1L, name = "No Data", numeric_value = NULL, missing = TRUE)
        ))
    )

    # backwards compatilbity with old class names
    class(labelled) <- c("labelled_spss", "labelled")

    # hack to make the the is.na method act like the old one if we have a newer
    # haven
    if (packageVersion("haven") > "1.1.2") {
        is.na.labelled_spss <<- haven:::is.na.haven_labelled_spss
    }
    expect_equivalent(
        toVariable(labelled),
        list(values = rep(1:3, 3), type = "categorical", categories = list(
            list(id = 1L, name = "A", numeric_value = 1L, missing = TRUE),
            list(id = 2L, name = "B", numeric_value = 2L, missing = FALSE),
            list(id = 3L, name = "C", numeric_value = 3L, missing = FALSE),
            list(id = -1L, name = "No Data", numeric_value = NULL, missing = TRUE)
        ))
    )

    # even if only some values are labelled, the values are still used
    labelled <- haven::labelled_spss(rep(LETTERS[1:3], 3),
        structure(LETTERS[2],
            names = LETTERS[2]
        ),
        na_values = LETTERS[1]
    )
    expect_equivalent(
        toVariable(labelled),
        list(values = rep(1:3, 3), type = "categorical", categories = list(
            list(id = 1L, name = "A", numeric_value = 1L, missing = TRUE),
            list(id = 2L, name = "B", numeric_value = 2L, missing = FALSE),
            list(id = 3L, name = "C", numeric_value = 3L, missing = FALSE),
            list(id = -1L, name = "No Data", numeric_value = NULL, missing = TRUE)
        ))
    )


    # If the values are numeric, we still get a categorical
    labelled <- haven::labelled_spss(rep(1:3, 3),
        structure(2,
            names = LETTERS[2]
        ),
        na_values = 1
    )
    expect_equivalent(
        toVariable(labelled),
        list(values = rep(1:3, 3), type = "categorical", categories = list(
            list(id = 1L, name = "1", numeric_value = 1L, missing = TRUE),
            list(id = 2L, name = "2", numeric_value = 2L, missing = FALSE),
            list(id = 3L, name = "3", numeric_value = 3L, missing = FALSE),
            list(id = -1L, name = "No Data", numeric_value = NULL, missing = TRUE)
        ))
    )
})

test_that("toVariable handles duplicate factor levels", {
    ## Duplicate factor labels were deprecated and are forbidden in the `factor`
    ## constructor function starting in R 3.4.0, but create one anyway in case
    ## older versions are encountered, and because it apparently is still
    ## technically possible to create one like this:
    v <- structure(1:4, .Label = c("a", "b", "b", "c"), class = "factor")
    expect_warning(
        expect_equivalent(
            toVariable(v),
            list(values = 1:4, type = "categorical", categories = list(
                list(id = 1L, name = "a", numeric_value = 1L, missing = FALSE),
                list(id = 2L, name = "b", numeric_value = 2L, missing = FALSE),
                list(id = 3L, name = "b  (1)", numeric_value = 3L, missing = FALSE),
                list(id = 4L, name = "c", numeric_value = 4L, missing = FALSE),
                list(id = -1L, name = "No Data", numeric_value = NULL, missing = TRUE)
            ))
        ),
        "Duplicate factor levels given: disambiguating them in translation to Categorical type"
    )
})
test_that("categoriesFromLevels parses levels correctly", {
    expect_identical(
        categoriesFromLevels(levels(iris$Species)),
        list(
            list(id = 1L, name = "setosa", numeric_value = 1L, missing = FALSE),
            list(id = 2L, name = "versicolor", numeric_value = 2L, missing = FALSE),
            list(id = 3L, name = "virginica", numeric_value = 3L, missing = FALSE),
            .no.data
        )
    )
})
test_that("toVariable parses R Date class", {
    expect_equivalent(
        toVariable(as.Date(c("2014-12-16", "2014-12-17"))),
        list(
            values = c("2014-12-16", "2014-12-17"), type = "datetime",
            resolution = "D"
        )
    )
})

test_that("toVariable handles POSIX datetimes (and timezones)", {
    numtime <- 1454238117.123

    expect_equivalent(
        toVariable(as.POSIXct(numtime, origin = "1970-01-01", tz = "UTC")),
        list(
            values = "2016-01-31T11:01:57.123", type = "datetime",
            resolution = "ms"
        )
    )

    # We store times as UTC when they go between R and crunch's database, but we assume
    # they actually refer to local times, so when we convert a variable of eg 5AM central
    # time it gets stored as 5AM UTC
    expect_equivalent(
        toVariable(as.POSIXct(numtime, origin = "1970-01-01", tz = "America/Chicago")),
        list(
            values = "2016-01-31T05:01:57.123", type = "datetime",
            resolution = "ms"
        )
    )

    expect_equivalent(
        toVariable(as.POSIXct(numtime, origin = "1970-01-01", tz = "America/New_York")),
        list(
            values = "2016-01-31T06:01:57.123", type = "datetime",
            resolution = "ms"
        )
    )
})

test_that("POSTNewVariable rejects invalid categories", {
    expect_error(
        POSTNewVariable(
            "",
            list(
                type = "categorical", name = "bad ids",
                categories = list(
                    list(id = -1L, name = "B", numeric_value = 1L, missing = FALSE),
                    list(id = 2L, name = "C", numeric_value = 2L, missing = FALSE),
                    list(id = -1L, name = "No Data", numeric_value = NULL, missing = TRUE)
                )
            )
        ),
        "Invalid category ids: must be unique"
    )
    expect_error(
        POSTNewVariable(
            "",
            list(
                type = "categorical", name = "bad names",
                categories = list(
                    list(id = 1L, name = "Name 1", numeric_value = 1L, missing = FALSE),
                    list(id = 2L, name = "Name 1", numeric_value = 2L, missing = FALSE),
                    list(id = -1L, name = "No Data", numeric_value = NULL, missing = TRUE)
                )
            )
        ),
        "Invalid category names: must be unique"
    )
})

test_that("checkVarDefErrors errors correctly", {
    test_errs <- lapply(list("a", "b", 29), function(x) try(log(x), silent = TRUE))
    expect_error(
        checkVarDefErrors(test_errs),
        "The following variable definitions errored on upload: 1, 2"
    )
    test_errs <- lapply(list(29, 23, 24), function(x) try(log(x), silent = TRUE))
    expect_silent(checkVarDefErrors(test_errs))
})

with_mock_crunch({
    ds <- cachedLoadDataset("test ds")
    test_that("assignment restrictions", {
        expect_error(
            ds[[2]] <- 1:25,
            "Only character \\(name\\) indexing supported"
        )
    })
    test_that("Input length validation", {
        expect_error(
            ds$newvar <- 1:13,
            "replacement has 13 rows, data has 25"
        )
        expect_error(
            ds$newvar <- rep(6, 11),
            "replacement has 11 rows, data has 25"
        )
    })

    test_that("Adding a variable with all the same values gets sent more concisely", {
        expect_POST(
            ds$newvar <- rep(5, 25),
            "https://app.crunch.io/api/datasets/1/variables/",
            '{"values":5,"type":"numeric","name":"newvar","alias":"newvar"}'
        )
    })

    test_that("Adding a variable with all missing doesn't send 'values'", {
        expect_POST(
            ds$newvar <- NA_real_,
            "https://app.crunch.io/api/datasets/1/variables/",
            '{"type":"numeric","name":"newvar","alias":"newvar"}'
        )
        expect_POST(
            ds$newvar <- rep(NA_real_, 25),
            "https://app.crunch.io/api/datasets/1/variables/",
            '{"type":"numeric","name":"newvar","alias":"newvar"}'
        )
    })
})

with_test_authentication({
    ds <- newDataset(df)
    test_that("addVariable creates a new remote numeric variable", {
        ds <- addVariables(
            ds,
            VariableDefinition(df$v3, name = "New var", alias = "newVar")
        )
        expect_true("newVar" %in% names(ds))
        nv <- ds$newVar
        expect_true(is.Numeric(nv))
        expect_true(is.Numeric(ds[["v3"]]))
        expect_identical(as.vector(nv), as.vector(ds$v3))
    })
    test_that("addVariable creates text variables from character", {
        ds <- addVariables(
            ds,
            VariableDefinition(df$v2, name = "New var2", alias = "newVar2")
        )
        expect_true("newVar2" %in% names(ds))
        nv <- ds$newVar2
        expect_true(is.Text(nv))
        expect_identical(
            as.vector(nv)[1:15],
            as.vector(ds$v2)[1:15]
        )
        ## note that NAs aren't getting caught in the CSV importer
        ## anymore, but they're right in the addVariable method
    })
    test_that("addVariable creates categorical from factor", {
        ds <- addVariables(
            ds,
            VariableDefinition(df$v4, name = "New var3", alias = "newVar3")
        )
        expect_true("newVar3" %in% names(ds))
        nv <- ds$newVar3
        expect_true(is.Categorical(nv))
        expect_identical(as.vector(nv), as.vector(ds$v4))
    })
    test_that("addVariable creates datetime from Date", {
        ds <- addVariables(
            ds,
            VariableDefinition(df$v5, name = "New var4", alias = "newVar4")
        )
        expect_true("newVar4" %in% names(ds))
        nv <- ds$newVar4
        expect_true(is.Datetime(nv))
        expect_identical(as.vector(nv), as.vector(ds$v5))
    })
    test_that("addVariable creates datetime from POSIXct", {
        skip("Can't support POSIXt until the app supports timezones")
        ds <- addVariables(ds, VariableDefinition(as.POSIXct(df$v5),
            name = "New var 5", alias = "newVar5"
        ))
        expect_true("newVar5" %in% names(ds))
        nv <- ds$newVar5
        expect_true(is.Datetime(nv))
        expect_identical(as.vector(nv), as.vector(ds$v5))
    })
    test_that("[[<- adds variables", {
        ds$newvariable <- 20:1
        expect_true(is.Numeric(ds$newvariable))
        expect_identical(mean(ds$newvariable), 10.5)
    })
    test_that("Variable lengths must match, in an R way", {
        expect_error(
            ds[["not valid"]] <- 1:7,
            "replacement has 7 rows, data has 20"
        )
        ds$ok <- 1
        expect_identical(as.vector(ds$ok), rep(1, 20))
    })

    test_that("Adding text variables (debugging)", {
        ds <- newDataset(data.frame(x = 1:1024))
        ds$a_text_var <- "12345 Some text that is definitely >4 characters"
        ds$a_factor <- factor("Different text")
        ds$the_name <- name(ds)
        ds$another <- factor(rep(c(NA, "Longer text"), 512))
        expect_true(is.Text(ds$a_text_var))
        expect_true(is.Categorical(ds$a_factor))
        expect_true(is.Text(ds$the_name))
        expect_true(is.Categorical(ds$another))
        expect_equal(
            as.array(crtabs(~a_text_var, data = ds)),
            array(1024L,
                dim = 1L,
                dimnames = list(a_text_var = "12345 Some text that is definitely >4 characters")
            )
        )
        expect_equal(
            as.array(crtabs(~a_factor, data = ds)),
            array(1024L,
                dim = 1L,
                dimnames = list(a_factor = "Different text")
            )
        )
        expect_equal(
            as.array(crtabs(~the_name, data = ds)),
            array(1024L,
                dim = 1L,
                dimnames = list(the_name = name(ds))
            )
        )
        expect_equal(
            as.array(crtabs(~another, data = ds, useNA = "always")),
            array(c(512L, 512L),
                dim = 2L,
                dimnames = list(another = c("Longer text", "No Data"))
            )
        )
        expect_identical(
            head(as.vector(ds$a_text_var), 1),
            "12345 Some text that is definitely >4 characters"
        )
    })

    ds <- newDataset(df)
    test_that("Categorical to R and back", {
        v4 <- as.vector(ds$v4)
        expect_identical(levels(v4), c("B", "C"))
        ds$v4a <- v4
        expect_equivalent(as.vector(ds$v4), as.vector(ds$v4a))
    })
    exclusion(ds) <- ds$v3 == 10
    test_that("Categorical to R and back with an exclusion", {
        v4b <- as.vector(ds$v4)
        expect_identical(levels(v4b), c("B", "C"))
        expect_length(v4b, 19)
        ds$v4b <- v4b
        expect_equivalent(as.vector(ds$v4b), as.vector(ds$v4a))
    })
})
