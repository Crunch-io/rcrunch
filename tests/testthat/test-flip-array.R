context("Flipping array subvariables")

with_test_authentication({
    ds <- newDatasetFromFixture("apidocs")

    ## Deep copy array, rename it, etc.
    ## Hey, let's add another subvar to it too!
    ds$petloc2 <- copy(ds$petloc, deep=TRUE, name="Other pet loc")
    aliases(subvariables(ds$petloc2)) <- c("p12_a", "p12_b")
    ds$petloc2 <- addSubvariable(ds$petloc2, ds$q1)

    newvars <- flipArrays(ds[c("petloc", "petloc2")])
    test_that("We get VarDefs", {
        expect_length(newvars, 3)
        expect_true(all(vapply(newvars, inherits, logical(1),
            what="VariableDefinition")))
    })
    ds <- addVariables(ds, newvars)
    test_that("The flipped variables are created", {
        print(names(ds))
        expect_true(all(c("Home, flipped", "Work, flipped", "Pet, flipped") %in% names(variables(ds))))
        expect_true(all(c("Home, flipped", "Work, flipped", "Pet, flipped") %in% names(ds)))
        expect_identical(names(subvariables(ds[["Home, flipped"]])),
            c("Pets by location", "Other pet loc"))
        expect_identical(names(subvariables(ds[["Pet, flipped"]])),
            "Other pet loc")
    })
})
