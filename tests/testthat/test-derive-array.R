context("Deriving array variables")

with_test_authentication({
    ds <- newDatasetFromFixture("apidocs")
    vd <- deriveArray(list(ds$q1, ds$petloc$petloc_home), name="Derived pets")
    test_that("deriveArray returns a VarDef", {
        expect_is(vd, "VariableDefinition")
    })
    ds$derivedarray <- vd
    test_that("Sending a derived array vardef creates a derived array", {
        expect_true(is.CA(ds$derivedarray))
        expect_identical(names(subvariables(ds$derivedarray)), c("Pet", "Home"))
        expect_identical(as.vector(ds$derivedarray[[1]]), as.vector(ds$q1))
    })

    ## Try editing metadata of the derived array, check that it updates but not parents

    ## Try editing values, see what happens?

    ## Try making derived array from derived array

    ## Deep copy array, rename it, etc., and make a "flipped" version

    ## Make derived array of derived array

    ## Append to that dataset, make sure all arrays update appropriately

    ## TODO: more things (filter entities, drop rows, etc.)
})
