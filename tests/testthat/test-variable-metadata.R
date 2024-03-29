context("All variable metadata")

with_mock_crunch({
    ds <- cachedLoadDataset("test ds")
    vm <- variableMetadata(ds)
    genders <- categories(ds$gender)
    test_that("variableMetadata exists, is a catalog", {
        expect_is(vm, "VariableCatalog")
        expect_identical(aliases(vm), aliases(allVariables(ds)))
        mymr <- index(vm)[[which(aliases(vm) == "mymrset")]]
        expect_identical(
            mymr$subvariables,
            c(
                "https://app.crunch.io/api/datasets/1/variables/mymrset/subvariables/subvar2/",
                "https://app.crunch.io/api/datasets/1/variables/mymrset/subvariables/subvar1/",
                "https://app.crunch.io/api/datasets/1/variables/mymrset/subvariables/subvar3/"
            )
        )
    })
})

# It does make internet request because variables catalog is lazy, unless we force it
ds <- forceVariableCatalog(ds)
httpcache::clearCache()
without_internet({
    test_that("Getting categories and subvariables from variableMetadata doesn't make a request", {
        ds2 <- ds
        ds2@variables <- vm
        expect_identical(categories(ds2$gender), genders)
        expect_identical(
            names(subvariables(ds2$mymrset)),
            c("First", "Second", "Last")
        )
    })
})

with_test_authentication({
    test_that("variableMetadata on apidocs dataset", {
        ds <- newDatasetFromFixture("apidocs")
        vm <- variableMetadata(ds)
        expect_is(vm, "VariableCatalog")
        i <- which(aliases(vm) == "allpets")
        expect_identical(
            Categories(data = vm[[i]]$categories),
            categories(ds$allpets)
        )
        expect_identical(
            vm[[i]]$subvariables,
            subvariableURLs(tuple(ds$allpets))
        )
        expect_true(all(grepl("^http", urls(vm))))
        expect_true(!any(is.na(getIndexSlot(vm, "id"))))
    })
})
