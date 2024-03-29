context("Deleting variables")

test_that("delete() method fallback", {
    expect_error(delete(NULL), "'delete' only valid for Crunch objects")
    expect_error(delete(list()), "'delete' only valid for Crunch objects")
})

with_mock_crunch({
    ds <- cachedLoadDataset("test ds")
    test_that("Assigning NULL doesn't ask you about deleting 0 variables", {
        expect_message(
            ds$NOTAVARIABLE <- df$NOTAVARIABLE,
            paste(dQuote("NOTAVARIABLE"), "is not a variable; nothing to delete by assigning NULL")
        )
    })
    test_that("Delete methods require consent", {
        expect_error(
            delete(ds$gender),
            "Must confirm deleting variable"
        )
        expect_error(
            ds$gender <- NULL,
            "Must confirm deleting variable"
        )
        expect_error(
            ds[[1]] <- NULL,
            "Must confirm deleting variable"
        )
        expect_error(
            ds$mymrset <- NULL,
            "Must confirm deleting variable"
        )
        expect_error(
            deleteVariables(ds, "gender"),
            "Must confirm deleting variable"
        )
        expect_error(
            deleteVariables(ds, c("gender", "birthyr")),
            "Must confirm deleting variable"
        )
    })
    test_that("deleteSubvariable also requires consent", {
        expect_error(
            deleteSubvariable(ds$mymrset, "subvar1"),
            "Must confirm deleting subvariable"
        )
    })
    with(consent(), {
        test_that("If consent given, all of these methods do DELETE", {
            expect_DELETE(
                delete(ds$gender),
                "https://app.crunch.io/api/datasets/1/variables/gender/"
            )
            expect_DELETE(
                ds$gender <- NULL,
                "https://app.crunch.io/api/datasets/1/variables/gender/"
            )
            expect_DELETE(
                ds$mymrset <- NULL,
                "https://app.crunch.io/api/datasets/1/variables/mymrset/"
            )
            expect_DELETE(
                ds[[2]] <- NULL,
                "https://app.crunch.io/api/datasets/1/variables/gender/"
            )
            expect_DELETE(
                deleteVariables(ds, "gender"),
                "https://app.crunch.io/api/datasets/1/variables/gender/"
            )
            expect_DELETE(
                deleteVariables(ds, c("gender", "birthyr")),
                "https://app.crunch.io/api/datasets/1/variables/gender/"
            )
        })

        test_that("deleteSubvariable deletes the subvariable", {
            expect_DELETE(
                deleteSubvariable(ds$mymrset, "subvar1"),
                "https://app.crunch.io/api/datasets/1/variables/mymrset/subvariables/subvar1/"
            )
            expect_DELETE(
                deleteSubvariable(ds$mymrset, 2),
                "https://app.crunch.io/api/datasets/1/variables/mymrset/subvariables/subvar1/"
            )
            expect_DELETE(
                deleteSubvariables(ds$mymrset, 2),
                "https://app.crunch.io/api/datasets/1/variables/mymrset/subvariables/subvar1/"
            )
        })
    })
})

with_test_authentication({
    whereas("Attempting to delete variables by various means", {
        ds <- newDataset(df)
        v1 <- ds$v1
        test_that("Setup", {
            expect_valid_df_import(ds)
        })

        with_consent(ds <- deleteVariable(ds, c("v1", "v4")))
        test_that("deleteVariable(s) removes variables", {
            expect_false(any(c("v1", "v4") %in% names(ds)))
        })
        test_that("Refreshing a deleted variable errors informatively", {
            expect_error(
                refresh(v1),
                "Variable not found. It may have been deleted."
            )
        })

        with_consent(ds <- deleteVariable(ds, c("v6", "v6", "v5")))
        test_that("deleteVariables ignores duplicates variables", {
            expect_false(any(c("v6", "v5") %in% names(ds)))
        })

        with_consent(ds$v3 <- NULL)
        test_that("Assigning NULL removes variables", {
            expect_null(ds$v3)
            expect_null(refresh(ds)$v3)
        })
    })

    whereas("Attepmting to delete arrays and subvariables", {
        ds <- newDatasetFromFixture("apidocs")

        test_that("Array variables are fully deleted", {
            expect_true("petloc" %in% names(ds))
            expect_false("petloc_home" %in% names(ds))
            with_consent(ds$petloc <- NULL)
            expect_false("petloc" %in% names(ds))
            expect_false("petloc_home" %in% names(ds))
        })

        cats <- categories(ds$allpets)
        cats[[3]]$selected <- TRUE
        categories(ds$allpets) <- cats
        test_that("Delete MR subvariable with multiple 'selected' attributes", {
            expect_identical(
                names(Filter(is.selected, categories(ds$allpets))),
                c("selected", "not asked")
            ) ## There are two selected
            with_consent(ds$allpets <- deleteSubvariable(ds$allpets, "allpets_2"))
            expect_true("allpets" %in% names(ds))
            expect_true(is.MR(ds$allpets))
            expect_false("allpets_2" %in% names(ds))
            expect_false("allpets_2" %in% aliases(subvariables(ds$allpets)))
            expect_identical(
                names(subvariables(ds$allpets)),
                c("Cat", "Bird")
            )
        })
    })
})
