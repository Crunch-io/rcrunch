context("All variable metadata")

with_mock_HTTP({
    ds <- loadDataset("test ds")
    test_that("variableMetadata", {
        vm <- variableMetadata(ds, parent=TRUE)
        expect_is(vm, "VariableCatalog")
        expect_identical(aliases(vm),
            c("gender", "birthyr", "starttime", "mymrset", "catarray",
            "textVar", "subvar2", "subvar1", "subvar3"))
        i <- which(aliases(vm) == "gender")
        expect_identical(Categories(data=vm[[i]]$categories),
            categories(ds$gender))
    })

    test_that("are_subvars on variableMetadata", {

    })
})

with_test_authentication({
    test_that("variableMetadata on apidocs dataset", {
        ds <- newDatasetFromFixture("apidocs")
        vm <- variableMetadata(ds)
        expect_is(vm, "VariableCatalog")
        i <- which(aliases(vm) == "allpets")
        expect_identical(Categories(data=vm[[i]]$categories),
            categories(ds$allpets))
        expect_identical(vm[[i]]$subvariables,
            subvariables(tuple(ds$allpets)))
        expect_true(all(grepl("^http", urls(vm))))
        expect_true(!any(is.na(getIndexSlot(vm, "id"))))
    })
})
