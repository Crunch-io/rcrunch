context("Hiding variables")

with_mock_HTTP({
    ds <- loadDataset("ECON.sav")
    test_that("hiddenVariables", {
        expect_identical(hiddenVariables(ds), "Birth Year")
        expect_identical(hiddenVariables(ds, "alias"), "birthyr")
    })

    test_that("Can subset dataset with hidden variable by name/alias", {
        ds_sub <- ds[c("gender", "birthyr")]
        expect_identical(names(ds_sub), "gender")
        expect_identical(aliases(allVariables(ds_sub)), c("gender", "birthyr"))
    })

    test_that("Can delete a hidden variable", {
        ## Circumvent the try-catch inside deleteVariables
        with_mock(try=function (expr, silent) force(expr), eval.parent({
            expect_message(
                expect_DELETE(ds$birthyr <- NULL,
                    "/api/datasets/dataset3/variables/birthyr.json"),
                NA)
            expect_message(
                expect_DELETE(deleteVariables(ds, "birthyr"),
                    "/api/datasets/dataset3/variables/birthyr.json"),
                NA)
        }))
        skip_on_jenkins("No idea why this fails to catch the warning on Jenkins but not on Travis or locally")
        expect_warning(
            expect_DELETE(delete(ds$birthyr),
                "/api/datasets/dataset3/variables/birthyr.json"),
            "Variable birthyr is hidden")
    })
})

if (run.integration.tests) {
    with_test_authentication({
        with(test.dataset(df), {
            var1 <- ds[[1]]
            test_that("Hide and unhide method for variables", {
                expect_true(name(var1) %in% names(variables(ds)))
                var1 <- hide(var1)
                ds <- refresh(ds)
                expect_false(name(var1) %in% names(variables(ds)))

                var1 <- unhide(var1)
                ds <- refresh(ds)
                expect_true(name(var1) %in% names(variables(ds)))
            })
        })

        with(test.dataset(df), {
            test_that("There are no hidden variables to start", {
                expect_equivalent(index(hidden(ds)), list())
                expect_identical(hiddenVariables(ds), c())
                expect_identical(dim(ds), dim(df))
            })

            try(ds <- hideVariables(ds, c("v2", "v3")))
            test_that("hideVariables hides by alias", {
                expect_identical(names(ds)[1:2], c("v1", "v4"))
                expect_identical(hiddenVariables(ds), c("v2", "v3"))
                expect_length(hidden(ds), 2)
                expect_length(variables(ds), ncol(df) - 2)
                expect_identical(dim(ds), c(nrow(df), ncol(df) - 2L))
            })

            try(hiddenVariables(ds) <- "v3")
            ## work like is.na<-, i.e. adds but doesn't unhide by omitting
            test_that("hiddenVariables<- does nothing if already hidden", {
                expect_identical(hiddenVariables(ds), c("v2", "v3"))
                expect_identical(names(ds)[1:2], c("v1", "v4"))
                expect_identical(dim(ds), c(nrow(df), ncol(df) - 2L))
            })

            try(hiddenVariables(ds) <- "v4")
            test_that("hiddenVariables<- adds variables", {
                expect_identical(names(ds)[1:2], c("v1", "v5"))
                expect_identical(hiddenVariables(ds), c("v2", "v3", "v4"))
                expect_identical(dim(ds), c(nrow(df), ncol(df) - 3L))
            })

            test_that("hidden variables can be accessed with $", {
                expect_warning(z <- ds$v2, "hidden")
                expect_true(is.Text(z))
            })

            try(ds <- unhideVariables(ds, c("v2", "v3", "v4")))

            test_that("unhideVariables by alias", {
                expect_identical(hiddenVariables(ds), c())
                expect_identical(dim(ds), dim(df))
                expect_warning(ds$v2, NA)
                expect_true(is.Text(ds$v2))
            })
        })

        with(test.dataset(df), {
            test_that("hideVariables with grep is deprecated (and by index)", {
                ds <- hideVariables(ds, c(2, 3))
                expect_identical(names(ds)[1:2], c("v1", "v4"))

                expect_warning(ds <- unhideVariables(ds, pattern="v[23]"),
                    "Deprecation warning")
                expect_identical(hiddenVariables(ds), c())
            })

            test_that("Error handling", {
                expect_identical(hiddenVariables(ds), c()) # To be clear
                ## Need something better than subscript out of bounds, probably
            })
        })

        with(test.dataset(df), {
            test_that("can hide variables by group", {
                ordering(ds) <- VariableOrder(
                    VariableGroup(name="g1", variables=list(ds$v1)),
                    VariableGroup(name="group2", variables=ds[c("v3", "v4")])
                )
                expect_length(grouped(ordering(ds)), 2)
                ds <- hideVariables(ds, ungrouped(ordering(ds)))
                expect_length(hiddenVariables(ds), ncol(df) - 3)
                expect_true(all(c("v2", "v5") %in% hiddenVariables(ds)))
            })
        })

        with(test.dataset(mrdf), {
            ds <- mrdf.setup(ds)
            test_that("Can hide array variables", {
                expect_true("CA" %in% names(ds))
                try(hiddenVariables(ds) <- c("CA", "v4"))
                expect_false("CA" %in% names(ds))
            })
        })
        with(test.dataset(mrdf), {
            ds <- mrdf.setup(ds, selections="1.0")
            test_that("Can hide MR variables", {
                expect_true("MR" %in% names(ds))
                try(hiddenVariables(ds) <- "MR")
                expect_false("MR" %in% names(ds))
            })
        })

        with(test.dataset(mrdf), {
            ds <- mrdf.setup(ds, pattern="mr_1")
            test_that("Can hide array variables even if they only have one subvar", {
                expect_identical(names(ds), c("CA", "mr_2", "mr_3", "v4"))
                expect_length(subvariables(ds$CA), 1)
                try(hiddenVariables(ds) <- "CA")
                expect_false("CA" %in% names(ds))
            })
        })
    })
}
