context("Deleting variables")

with_mock_HTTP({
    ds <- loadDataset("test ds")
    test_that("Assigning NULL doesn't ask you about deleting 0 variables", {
        expect_message(ds$NOTAVARIABLE <- df$NOTAVARIABLE,
            paste(dQuote("NOTAVARIABLE"), "is not a variable; nothing to delete by assigning NULL"))
    })
    with(temp.option(crunch.require.confirmation=TRUE), {
        test_that("Delete methods require consent", {
            expect_error(delete(ds$gender),
                "Must confirm deleting variable")
            expect_error(ds$gender <- NULL,
                "Must confirm deleting variable")
            expect_error(ds[[1]] <- NULL,
                "Must confirm deleting variable")
            expect_error(ds$mymrset <- NULL,
                "Must confirm deleting variable")
            expect_error(deleteVariables(ds, "gender"),
                "Must confirm deleting variable")
            expect_error(deleteVariables(ds, c("gender", "birthyr")),
                "Must confirm deleting variable")
        })
        test_that("deleteSubvariable also requires consent", {
            expect_error(deleteSubvariable(ds$mymrset, "subvar1"),
                "Must confirm deleting subvariable")
        })
        with(consent(), {
            test_that("If consent given, all of these methods do DELETE", {
                expect_DELETE(delete(ds$gender),
                    "api/datasets/1/variables/gender/")
                expect_DELETE(ds$gender <- NULL,
                    "api/datasets/1/variables/gender/")
                expect_DELETE(ds$mymrset <- NULL,
                    "api/datasets/1/variables/mymrset/")
                expect_DELETE(ds[[2]] <- NULL,
                    "api/datasets/1/variables/gender/")
                expect_DELETE(deleteVariables(ds, "gender"),
                    "api/datasets/1/variables/gender/")
                expect_DELETE(deleteVariables(ds, c("gender", "birthyr")),
                    "api/datasets/1/variables/gender/")
            })
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

        ds <- deleteVariable(ds, c("v1", "v4"))
        test_that("deleteVariable(s) removes variables", {
            expect_false(any(c("v1", "v4") %in% names(ds)))
        })
        test_that("Refreshing a deleted variable errors informatively", {
            expect_error(refresh(v1),
                "Variable not found. It may have been deleted.")
        })

        ds$v3 <- NULL
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
            ds$petloc <- NULL
            expect_false("petloc" %in% names(ds))
            expect_false("petloc_home" %in% names(ds))
        })

        cats <- categories(ds$allpets)
        cats[[3]]$selected <- TRUE
        categories(ds$allpets) <- cats
        test_that("Delete MR subvariable with multiple 'selected' attributes", {
            expect_identical(names(Filter(is.selected, categories(ds$allpets))),
                c("selected", "not asked")) ## There are two selected
            ## TODO: when deleteSubvariable doesn't unbind/rebind, assign it back
            deleteSubvariable(ds$allpets, "allpets_2")
            ds <- refresh(ds)
            expect_true("allpets" %in% names(ds))
            expect_true(is.MR(ds$allpets))
            expect_false("allpets_2" %in% names(ds))
            expect_false("allpets_2" %in% aliases(subvariables(ds$allpets)))
            expect_identical(names(subvariables(ds$allpets)),
                c("Cat", "Bird"))
        })
    })
})
