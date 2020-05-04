context("Private variables")

with_mock_crunch({
    ds <- loadDataset("Private test")
    test_that("privateVariables", {
        expect_identical(privateVariables(ds, "name"), "ID Number")
        expect_identical(privateVariables(ds), "id")
        with(temp.option(crunch.namekey.dataset = "name"), {
            expect_identical(privateVariables(ds), "ID Number")
        })
    })

    test_that("Can subset dataset with private variable by name/alias", {
        ds_sub <- ds[c("gender", "id")]
        expect_identical(names(ds_sub), "gender")
        expect_identical(aliases(allVariables(ds_sub)), c("gender", "id"))
    })

    test_that("private variables can be accessed with $", {
        expect_warning(
            z <- ds$id,
            "Variable id is private"
        )
        expect_true(is.Numeric(z))
    })
    test_that("Option not to warn when accessing private variables", {
        with(temp.option(crunch.warn.private = FALSE), {
            expect_warning(ds$id, NA)
        })
    })

    test_that("Can delete a private variable", {
        skip_on_jenkins(paste0(
            "No idea why this fails to catch the warning on Jenkins but not on ",
            "Travis or locally"
        ))
        with_consent({
            expect_warning(
                expect_DELETE(
                    delete(ds$id),
                    "https://app.crunch.io/api/datasets/5/variables/id/"
                ),
                "Variable id is private"
            )
        })
    })

    privatize_gender <- paste0(
        "https://app.crunch.io/api/datasets/5/folders/secure/",
        " ",
        "{",
        '"element":"shoji:catalog",',
        '"index":{',
        '"https://app.crunch.io/api/datasets/5/variables/gender/":{}',
        "}",
        "}"
    )
    privatize_both <- paste0(
        "https://app.crunch.io/api/datasets/5/folders/secure/",
        " ",
        "{",
        '"element":"shoji:catalog",',
        '"index":{',
        '"https://app.crunch.io/api/datasets/5/variables/gender/":{},',
        '"https://app.crunch.io/api/datasets/5/variables/height_inches/":{}',
        "},",
        '"graph":[',
        '"https://app.crunch.io/api/datasets/5/variables/id/",',
        '"https://app.crunch.io/api/datasets/5/variables/gender/",',
        '"https://app.crunch.io/api/datasets/5/variables/height_inches/"',
        "]",
        "}"
    )
    test_that("privatizeVariables with one variable", {
        expect_PATCH(
            ds <- privatizeVariables(ds, "gender"),
            privatize_gender
        )
    })

    test_that("privatizeVariables handles duplicated inputs", {
        expect_PATCH(
            ds <- privatizeVariables(ds, c("gender", "gender")),
            privatize_gender
        )
    })
    test_that("privatizeVariables excludes already private variables", {
        expect_PATCH(
            ds <- privatizeVariables(ds, c("gender", "id")),
            privatize_gender
        )
    })
    test_that("privatizeVariables with multiple vars to private", {
        expect_PATCH(
            ds <- privatizeVariables(ds, c("gender", "height_inches")),
            privatize_both
        )
        expect_PATCH(
            ds <- privatizeVariables(ds, 1:2),
            privatize_both
        )
    })

    test_that("privateVariables<- request", {
        expect_PATCH(
            privateVariables(ds) <- "gender",
            privatize_gender
        )
        expect_PATCH(
            privateVariables(ds) <- c("gender", "id"),
            privatize_gender
        )
    })

    deprivatize_id <- paste0(
        "https://app.crunch.io/api/datasets/5/folders/",
        " ",
        "{",
        '"element":"shoji:catalog",',
        '"index":{',
        '"https://app.crunch.io/api/datasets/5/variables/id/":{}',
        "}",
        "}"
    )
    test_that("deprivatizeVariables with various input types makes the right request", {
        expect_PATCH(
            ds <- deprivatizeVariables(ds, "id"),
            deprivatize_id
        )
        expect_PATCH(
            ds <- deprivatizeVariables(ds, c("gender", "id")),
            deprivatize_id
        )
        expect_no_request(ds <- deprivatizeVariables(ds, c("gender", "height_inches")))
    })

    test_that("privatize method on variable makes right request", {
        expect_PATCH(
            privatize(ds$gender),
            privatize_gender
        )
        expect_warning(
            expect_no_request(privatize(ds$id)),
            "Variable id is private"
        )
    })

    test_that("deprivatize method on variable makes right request", {
        expect_warning(
            expect_PATCH(
                deprivatize(ds$id),
                deprivatize_id
            ),
            "Variable id is private"
        )
        expect_no_request(deprivatize(ds$gender))
    })

    test_that("'s' of privatise spellings work", {
        expect_PATCH(
            ds <- privatiseVariables(ds, "gender"),
            privatize_gender
        )
        expect_PATCH(
            ds <- privatise(ds$gender),
            privatize_gender
        )
        expect_PATCH(
            ds <- deprivatiseVariables(ds, "id"),
            deprivatize_id
        )
        expect_warning(
            expect_PATCH(
                deprivatise(ds$id),
                deprivatize_id
            ),
            "Variable id is private"
        )
    })
})

with_test_authentication({
    whereas("Privatize and deprivatize variables and checking that the remote dataset updates", {
        ds <- newDataset(df)

        test_that("There are no private variables to start", {
            expect_equivalent(index(private(ds)), list())
            expect_identical(privateVariables(ds), c())
        })

        ds <- privatizeVariables(ds, c("v2", "v3"))
        test_that("privatizeVariables privatizes by alias", {
            expect_identical(names(ds)[1:2], c("v1", "v4"))
            expect_identical(privateVariables(ds), c("v2", "v3"))
            expect_length(private(ds), 2)
            expect_length(variables(ds), ncol(df) - 2)
            expect_identical(dim(ds), c(nrow(df), ncol(df) - 2L))
        })

        privateVariables(ds) <- "v3"
        ## work like is.na<-, i.e. adds but doesn't deprivatize by omitting
        test_that("privateVariables<- does nothing if already private", {
            expect_identical(privateVariables(ds), c("v2", "v3"))
            expect_identical(names(ds)[1:2], c("v1", "v4"))
            expect_identical(dim(ds), c(nrow(df), ncol(df) - 2L))
        })

        privateVariables(ds) <- "v4"
        test_that("privateVariables<- adds variables", {
            expect_identical(names(ds)[1:2], c("v1", "v5"))
            expect_identical(privateVariables(ds), c("v2", "v3", "v4"))
            expect_identical(dim(ds), c(nrow(df), ncol(df) - 3L))
        })

        ds <- deprivatizeVariables(ds, c("v2", "v3", "v4"))

        test_that("deprivatizeVariables by alias", {
            expect_identical(privateVariables(ds), c())
            expect_identical(dim(ds), dim(df))
            expect_warning(ds$v2, NA)
            expect_true(is.Text(ds$v2))
        })


        test_that("Can privatize hidden variables and vice versa", {
            ds <- privatizeVariables(ds, "v2")
            ds <- hideVariables(ds, "v3")
            expect_equal(privateVariables(ds), "v2")
            expect_equal(hiddenVariables(ds), "v3")

            ds <- privatizeVariables(ds, "v3")
            ds <- hideVariables(ds, "v2")
            expect_equal(privateVariables(ds), "v3")
            expect_equal(hiddenVariables(ds), "v2")
            ds <- deprivatizeVariables(ds, "v3")
            ds <- unhideVariables(ds, "v2")
        })


    })
    whereas("Checking that array variables can be private", {
        ds <- newDatasetFromFixture("apidocs")

        test_that("Can privatize categorical array variables", {
            expect_true("petloc" %in% names(ds))
            privateVariables(ds) <- "petloc"
            expect_false("petloc" %in% names(ds))
        })

        test_that("Can oruvatuze MR variables", {
            expect_true("allpets" %in% names(ds))
            ds <- privateVariables(ds, "allpets")
            expect_false("allpets" %in% names(ds))
        })
    })

    test_that("Can privatize array variables even if they only have one subvar", {
        ds <- mrdf.setup(newDataset(mrdf[c(1, 4)]))
        expect_identical(names(ds), c("CA", "v4"))
        expect_length(subvariables(ds$CA), 1)
        privateVariables(ds) <- "CA"
        expect_false("CA" %in% names(ds))
    })
})
