context('Adding subvariables')

with_test_authentication({
    ds <- newDatasetFromFixture("apidocs")
    test_that("Adding a subvariable to an array", {
        ds$petloc_daycare <- VariableDefinition(factor(rep(c("Cat",
            "Dog"), 10)), name="doggy daycare")
        expect_identical(names(subvariables(ds$petloc)),
            c("Home", "Work"))
        expect_true("petloc_daycare" %in% aliases(variables(ds)))
        ds$petloc <- addSubvariable(ds$petloc, ds$petloc_daycare)
        expect_identical(names(subvariables(ds$petloc)),
            c("Home", "Work", "doggy daycare"))
    })

    test_that("Add more than one subvariable to an array", {
        expect_identical(names(subvariables(ds$allpets)),
            c("Cat", "Dog", "Bird"))
        ds$allpets_7 <- copy(ds$allpets$allpets_1, deep=TRUE, name="Turtle")
        ds$allpets_8 <- copy(ds$allpets$allpets_2, deep=TRUE, name="Gerbil")
        ds$allpets <- addSubvariables(ds$allpets, list(ds$allpets_7, ds$allpets_8))
        expect_identical(names(subvariables(ds$allpets)),
            c("Cat", "Dog", "Bird", "Turtle", "Gerbil"))
        ds$allpets_12 <- copy(ds$allpets$allpets_1, deep=TRUE, name="Coyote")
        ds$allpets_13 <- copy(ds$allpets$allpets_2, deep=TRUE, name="Cockroach")
        ds$allpets <- addSubvariables(ds$allpets, ds[c("allpets_12", "allpets_13")])
        expect_identical(names(subvariables(ds$allpets)),
            c("Cat", "Dog", "Bird", "Turtle", "Gerbil", "Coyote", "Cockroach"))
    })
})
