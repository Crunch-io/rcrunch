context("Private variables")

with_mock_crunch({
    ds <- loadDataset("private test data")
    test_that("privateVariables", {
        expect_identical(privateVariables(ds, "name"), "Phone number")
        expect_identical(privateVariables(ds), "phone")
        with(temp.option(crunch.namekey.dataset = "name"), {
            expect_identical(privateVariables(ds), "Phone number")
        })
    })

    test_that("Can subset dataset with private variable by name/alias", {
        ds_sub <- ds[c("food", "phone")]
        expect_identical(names(ds_sub), "food")
        expect_identical(aliases(allVariables(ds_sub)), c("food", "phone"))
    })

    test_that("private variables can be accessed with $", {
        expect_warning(
            z <- ds$phone,
            "Variable phone is private"
        )
        expect_true(is.Text(z))
    })
    test_that("Option not to warn when accessing private variables", {
        with(temp.option(crunch.warn.private = FALSE), {
            expect_warning(ds$phone, NA)
        })
    })

    test_that("Can delete a private variable", {
        with_consent({
            expect_warning(
                expect_DELETE(
                    delete(ds$phone),
                    "https://app.crunch.io/api/datasets/4/variables/phone/"
                ),
                "Variable phone is private"
            )
        })
    })

    private_food <- paste0(
        "https://app.crunch.io/api/datasets/4/folders/secure/",
        " ",
        "{",
        '"element":"shoji:catalog",',
        '"index":{',
        '"https://app.crunch.io/api/datasets/4/variables/food/":{}',
        "}",
        "}"
    )
    private_both <- paste0(
        "https://app.crunch.io/api/datasets/4/folders/secure/",
        " ",
        "{",
        '"element":"shoji:catalog",',
        '"index":{',
        '"https://app.crunch.io/api/datasets/4/variables/food/":{},',
        '"https://app.crunch.io/api/datasets/4/variables/city/":{}',
        "},",
        '"graph":[',
        '"https://app.crunch.io/api/datasets/4/variables/phone/",',
        '"https://app.crunch.io/api/datasets/4/variables/food/",',
        '"https://app.crunch.io/api/datasets/4/variables/city/"',
        "]",
        "}"
    )
    test_that("privatizeVariables with one variable to privatize", {
        expect_PATCH(
            ds <- privatizeVariables(ds, "food"),
            private_food
        )
    })
    test_that("privatizeVariables handles duplicated inputs", {
        expect_PATCH(
            ds <- privatizeVariables(ds, c("food", "food")),
            private_food
        )
    })
    test_that("privatizeVariables excludes already private variables", {
        expect_PATCH(
            ds <- privatizeVariables(ds, c("food", "phone")),
            private_food
        )
    })
    test_that("privatizeVariables with multiple vars to privatize", {
        expect_PATCH(
            ds <- privatizeVariables(ds, c("food", "city")),
            private_both
        )
        expect_PATCH(
            ds <- privatizeVariables(ds, 1:2),
            private_both
        )
    })

    test_that("privateVariables<- request", {
        expect_PATCH(
            privateVariables(ds) <- "food",
            private_food
        )
        expect_PATCH(
            privateVariables(ds) <- c("food", "phone"),
            private_food
        )
    })

    deprivatize_phone <- paste0(
        "https://app.crunch.io/api/datasets/4/folders/",
        " ",
        "{",
        '"element":"shoji:catalog",',
        '"index":{',
        '"https://app.crunch.io/api/datasets/4/variables/phone/":{}',
        "}",
        "}"
    )
    test_that("deprivatizeVariables with various input types makes the right request", {
        expect_PATCH(
            ds <- deprivatizeVariables(ds, "phone"),
            deprivatize_phone
        )
        expect_PATCH(
            ds <- deprivatizeVariables(ds, c("food", "phone")),
            deprivatize_phone
        )
        expect_no_request(ds <- deprivatizeVariables(ds, c("food", "city")))
    })

    test_that("privatize method on variable makes right request", {
        expect_PATCH(
            privatize(ds$food),
            private_food
        )
        expect_warning(
            expect_no_request(privatize(ds$phone)),
            "Variable phone is private"
        )
    })

    test_that("deprivatize method on variable makes right request", {
        expect_warning(
            expect_PATCH(
                deprivatize(ds$phone),
                deprivatize_phone
            ),
            "Variable phone is private"
        )
        expect_no_request(deprivatize(ds$food))
    })
})

with_test_authentication({
    whereas("Privatizing and deprivatizing variables and checking that the remote dataset updates", {
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
    })

    whereas("Checking that array variables can be privatized", {
        ds <- newDatasetFromFixture("apidocs")

        test_that("Can privatize categorical array variables", {
            expect_true("petloc" %in% names(ds))
            privateVariables(ds) <- "petloc"
            expect_false("petloc" %in% names(ds))
        })

        test_that("Can privatize MR variables", {
            expect_true("allpets" %in% names(ds))
            ds <- privatizeVariables(ds, "allpets")
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
