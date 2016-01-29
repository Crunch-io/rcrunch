context('Adding subvariables')

if (run.integration.tests) {
    with(test.authentication, {
        with(test.dataset(newDatasetFromFixture("apidocs")), {
<<<<<<< HEAD
            expect_false("doggy daycare" %in% names(subvariables(ds$petloc)))
            ds$petloc_daycare <- VariableDefinition(factor(rep(c("Cat", "Dog"), 10)), name="doggy daycare")
            expect_true('petloc_daycare' %in% aliases(variables(ds)))
            addSubvariable(ds$petloc, ds$petloc_daycare)
            ds <- refresh(ds)
            expect_true("doggy daycare" %in% names(subvariables(ds$petloc)))
            expect_identical(c("Home", "Work", "doggy daycare"), names(subvariables(ds$petloc)))
        })
        with(test.dataset(newDatasetFromFixture("apidocs")), {
            expect_false("doggy daycare" %in% names(subvariables(ds$petloc)))
            petloc_daycare <- VariableDefinition(factor(rep(c("Cat", "Dog"), 10)), name="doggy daycare", alias='petloc_daycare')
            expect_false('petloc_daycare' %in% aliases(variables(ds)))
            addSubvariable(ds$petloc, petloc_daycare, ds)
            ds <- refresh(ds)
            print(names(subvariables(ds$petloc)))
            expect_true("doggy daycare" %in% names(subvariables(ds$petloc)))
            expect_identical(c("Home", "Work", "doggy daycare"), names(subvariables(ds$petloc)))
=======
            test_that("Adding a subvariable to an array", {
                ds$petloc_daycare <- VariableDefinition(factor(rep(c("Cat",
                    "Dog"), 10)), name="doggy daycare")
                expect_identical(c("Home", "Work"),
                    names(subvariables(ds$petloc)))
                expect_true("petloc_daycare" %in% aliases(variables(ds)))
                addSubvariable(ds$petloc, ds$petloc_daycare)
                ds <- refresh(ds)
                expect_identical(c("Home", "Work", "doggy daycare"),
                    names(subvariables(ds$petloc)))
            })
>>>>>>> upstream/master
        })
    })
}
