context("All variable metadata")

with_mock_HTTP({
    ds <- loadDataset("test ds")
    test_that("variableMetadata", {
        vm <- variableMetadata(ds)
        expect_is(vm, "VariableCatalog")
        expect_identical(aliases(vm),
            c("gender", "birthyr", "starttime", "mymrset", "catarray", "textVar"))
        expect_identical(Categories(data=vm[[1]]$categories),
            categories(ds$gender))
        mymr <- index(vm)[[4]]
        expect_identical(mymr$subvariables,
            c("/api/datasets/dataset1/variables/subvar2/",
            "/api/datasets/dataset1/variables/subvar1/",
            "/api/datasets/dataset1/variables/subvar3/"))
        expect_identical(mymr$subreferences,
            list(list(name="First", alias="subvar2", description=NULL),
                list(name="Second", alias="subvar1", description=NULL),
                list(name="Last", alias="subvar3", description=NULL)))
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
