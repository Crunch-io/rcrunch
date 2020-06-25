context("Hiding variables")

with_mock_crunch({
    ds <- loadDataset("ECON.sav")
    test_that("hiddenVariables", {
        expect_identical(hiddenVariables(ds, "name"), "Birth Year")
        expect_identical(hiddenVariables(ds), "birthyr")
        with(temp.option(crunch.namekey.dataset = "name"), {
            expect_identical(hiddenVariables(ds), "Birth Year")
        })
    })

    test_that("Can subset dataset with hidden variable by name/alias", {
        ds_sub <- ds[c("gender", "birthyr")]
        expect_identical(names(ds_sub), "gender")
        expect_identical(aliases(allVariables(ds_sub)), c("gender", "birthyr"))
    })

    test_that("hidden variables can be accessed with $", {
        expect_warning(
            z <- ds$birthyr,
            "Variable birthyr is hidden"
        )
        expect_true(is.Numeric(z))
    })
    test_that("Option not to warn when accessing hidden variables", {
        with(temp.option(crunch.warn.hidden = FALSE), {
            expect_warning(ds$birthyr, NA)
        })
    })

    test_that("Can delete a hidden variable", {
        skip_on_jenkins(paste0(
            "No idea why this fails to catch the warning on Jenkins but not on ",
            "Travis or locally"
        ))
        with_consent({
            expect_warning(
                expect_DELETE(
                    delete(ds$birthyr),
                    "https://app.crunch.io/api/datasets/3/variables/birthyr/"
                ),
                "Variable birthyr is hidden"
            )
        })
    })

    hide_gender <- paste0(
        "https://app.crunch.io/api/datasets/3/folders/hidden/",
        " ",
        "{",
        '"element":"shoji:catalog",',
        '"index":{',
        '"https://app.crunch.io/api/datasets/3/variables/gender/":{}',
        "}",
        "}"
    )
    hide_both <- paste0(
        "https://app.crunch.io/api/datasets/3/folders/hidden/",
        " ",
        "{",
        '"element":"shoji:catalog",',
        '"index":{',
        '"https://app.crunch.io/api/datasets/3/variables/gender/":{},',
        '"https://app.crunch.io/api/datasets/3/variables/starttime/":{}',
        "},",
        '"graph":[',
        '"https://app.crunch.io/api/datasets/3/variables/birthyr/",',
        '"https://app.crunch.io/api/datasets/3/variables/gender/",',
        '"https://app.crunch.io/api/datasets/3/variables/starttime/"',
        "]",
        "}"
    )
    test_that("hideVariables with one variable to hide", {
        expect_PATCH(
            ds <- hideVariables(ds, "gender"),
            hide_gender
        )
    })
    test_that("hideVariables handles duplicated inputs", {
        expect_PATCH(
            ds <- hideVariables(ds, c("gender", "gender")),
            hide_gender
        )
    })
    test_that("hideVariables excludes already hidden variables", {
        expect_PATCH(
            ds <- hideVariables(ds, c("gender", "birthyr")),
            hide_gender
        )
    })
    test_that("hideVariables with multiple vars to hide", {
        expect_PATCH(
            ds <- hideVariables(ds, c("gender", "starttime")),
            hide_both
        )
        expect_PATCH(
            ds <- hideVariables(ds, 1:2),
            hide_both
        )
    })

    test_that("hiddenVariables<- request", {
        expect_PATCH(
            hiddenVariables(ds) <- "gender",
            hide_gender
        )
        expect_PATCH(
            hiddenVariables(ds) <- c("gender", "birthyr"),
            hide_gender
        )
    })

    unhide_birthyr <- paste0(
        "https://app.crunch.io/api/datasets/3/folders/",
        " ",
        "{",
        '"element":"shoji:catalog",',
        '"index":{',
        '"https://app.crunch.io/api/datasets/3/variables/birthyr/":{}',
        "}",
        "}"
    )
    test_that("unhideVariables with various input types makes the right request", {
        expect_PATCH(
            ds <- unhideVariables(ds, "birthyr"),
            unhide_birthyr
        )
        expect_PATCH(
            ds <- unhideVariables(ds, c("gender", "birthyr")),
            unhide_birthyr
        )
        expect_no_request(ds <- unhideVariables(ds, c("gender", "starttime")))
    })

    test_that("hide method on variable makes right request", {
        expect_PATCH(
            hide(ds$gender),
            hide_gender
        )
        expect_warning(
            expect_no_request(hide(ds$birthyr)),
            "Variable birthyr is hidden"
        )
    })

    test_that("unhide method on variable makes right request", {
        expect_warning(
            expect_PATCH(
                unhide(ds$birthyr),
                unhide_birthyr
            ),
            "Variable birthyr is hidden"
        )
        expect_no_request(unhide(ds$gender))
    })
})

with_test_authentication({
    whereas("Hiding and unhiding variables and checking that the remote dataset updates", {
        ds <- newDataset(df)

        test_that("There are no hidden variables to start", {
            expect_equivalent(index(hiddenFolder(ds)), list())
            expect_identical(hiddenVariables(ds), c())
        })

        ds <- hideVariables(ds, c("v2", "v3"))
        test_that("hideVariables hides by alias", {
            expect_identical(names(ds)[1:2], c("v1", "v4"))
            expect_identical(hiddenVariables(ds), c("v2", "v3"))
            expect_length(hiddenFolder(ds), 2)
            expect_length(variables(ds), ncol(df) - 2)
            expect_identical(dim(ds), c(nrow(df), ncol(df) - 2L))
        })

        hiddenVariables(ds) <- "v3"
        ## work like is.na<-, i.e. adds but doesn't unhide by omitting
        test_that("hiddenVariables<- does nothing if already hidden", {
            expect_identical(hiddenVariables(ds), c("v2", "v3"))
            expect_identical(names(ds)[1:2], c("v1", "v4"))
            expect_identical(dim(ds), c(nrow(df), ncol(df) - 2L))
        })

        hiddenVariables(ds) <- "v4"
        test_that("hiddenVariables<- adds variables", {
            expect_identical(names(ds)[1:2], c("v1", "v5"))
            expect_identical(hiddenVariables(ds), c("v2", "v3", "v4"))
            expect_identical(dim(ds), c(nrow(df), ncol(df) - 3L))
        })

        ds <- unhideVariables(ds, c("v2", "v3", "v4"))

        test_that("unhideVariables by alias", {
            expect_identical(hiddenVariables(ds), c())
            expect_identical(dim(ds), dim(df))
            expect_warning(ds$v2, NA)
            expect_true(is.Text(ds$v2))
        })
    })

    whereas("Checking that array variables can be hidden", {
        ds <- newDatasetFromFixture("apidocs")

        test_that("Can hide categorical array variables", {
            expect_true("petloc" %in% names(ds))
            hiddenVariables(ds) <- "petloc"
            expect_false("petloc" %in% names(ds))
        })

        test_that("Can hide MR variables", {
            expect_true("allpets" %in% names(ds))
            ds <- hideVariables(ds, "allpets")
            expect_false("allpets" %in% names(ds))
        })
    })

    test_that("Can hide array variables even if they only have one subvar", {
        ds <- mrdf.setup(newDataset(mrdf[c(1, 4)]))
        expect_identical(names(ds), c("CA", "v4"))
        expect_length(subvariables(ds$CA), 1)
        hiddenVariables(ds) <- "CA"
        expect_false("CA" %in% names(ds))
    })
})
