context("Flipping array subvariables")

with_test_authentication({
    ds <- newDatasetFromFixture("apidocs")

    ## Deep copy array, rename it, etc.
    ## Hey, let's add another subvar to it too!
    pl2 <- copy(ds$petloc, deep=TRUE, name="Other pet loc")
    pl2$subvariables[[3]] <- pl2$subvariables[[1]]
    pl2$subvariables[[1]]$alias <- "pl2_a"
    pl2$subvariables[[2]]$alias <- "pl2_b"
    pl2$subvariables[[3]]$alias <- "pl2_c"
    pl2$subvariables[[3]]$name <- "Pet store"
    pl2$values <- cbind(pl2$values, pl2$values[,1])
    ds$petloc2 <- pl2

    newvars <- flipArrays(ds[c("petloc", "petloc2")])
    test_that("We get VarDefs", {
        expect_length(newvars, 3)
        expect_true(all(vapply(newvars, inherits, logical(1),
            what="VariableDefinition")))
    })
    ds <- addVariables(ds, newvars)
    test_that("The flipped variables are created", {
        expect_true(all(c("Home, flipped", "Work, flipped", "Pet store, flipped") %in% names(variables(ds))))
        expect_true(all(c("Home, flipped", "Work, flipped", "Pet store, flipped") %in% names(ds)))
        expect_identical(names(subvariables(ds[["Home, flipped"]])),
            c("Pets by location", "Other pet loc"))
        expect_identical(names(subvariables(ds[["Pet store, flipped"]])),
            "Other pet loc")
    })
})
