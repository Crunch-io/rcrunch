context("Adding subvariables")

with_mock_crunch({
    ds <- cachedLoadDataset("test ds")
    test_that("Adding an existing var to an array", {
        expect_PATCH(
            addSubvariable(ds$mymrset, ds$gender),
            "https://app.crunch.io/api/datasets/1/variables/mymrset/subvariables/"
        )
    })
    test_that("Adding a VarDef as a subvar to an array", {
        vd <- VariableDefinition(factor("1.0"), name = "doggy daycare")
        expect_POST(
            addSubvariable(ds$mymrset, vd),
            "https://app.crunch.io/api/datasets/1/variables/"
        )
    })
    test_that("Can't add subvariables to a non-array", {
        expect_error(addSubvariable(ds$birthyr, ds$gender),
            "is.Array(variable) is not TRUE",
            fixed = TRUE
        )
    })
})

with_test_authentication({
    ds <- newDatasetFromFixture("apidocs")
    test_that("Adding a subvariable to an array", {
        ds$petloc_daycare <- VariableDefinition(factor(rep(c(
            "Cat",
            "Dog"
        ), 10)), name = "doggy daycare")
        expect_identical(
            names(subvariables(ds$petloc)),
            c("Home", "Work")
        )
        expect_true("petloc_daycare" %in% aliases(variables(ds)))
        ds$petloc <- addSubvariable(ds$petloc, ds$petloc_daycare)
        expect_identical(
            names(subvariables(ds$petloc)),
            c("Home", "Work", "doggy daycare")
        )
    })

    test_that("Add more than one subvariable to an array", {
        expect_identical(
            names(subvariables(ds$allpets)),
            c("Cat", "Dog", "Bird")
        )
        ds$allpets_7 <- copy(ds$allpets$allpets_1, deep = TRUE, name = "Turtle")
        ds$allpets_8 <- copy(ds$allpets$allpets_2, deep = TRUE, name = "Gerbil")
        ds$allpets <- addSubvariables(ds$allpets, list(ds$allpets_7, ds$allpets_8))
        expect_identical(
            names(subvariables(ds$allpets)),
            c("Cat", "Dog", "Bird", "Turtle", "Gerbil")
        )
        ds$allpets_12 <- copy(ds$allpets$allpets_1, deep = TRUE, name = "Coyote")
        ds$allpets_13 <- copy(ds$allpets$allpets_2, deep = TRUE, name = "Cockroach")
        ds$allpets <- addSubvariables(ds$allpets, ds[c("allpets_12", "allpets_13")])
        expect_identical(
            names(subvariables(ds$allpets)),
            c("Cat", "Dog", "Bird", "Turtle", "Gerbil", "Coyote", "Cockroach")
        )
    })
    ds <- refresh(ds)
    test_that("Variable definitions can be added to an array", {
        sub1 <- toVariable(factor(seq_len(nrow(ds))), name = "Ant")
        ds$allpets <- addSubvariables(ds$allpets, sub1)
        expect_true("Ant" %in% names(subvariables(ds$allpets)))

        sub2 <- toVariable(factor(seq_len(nrow(ds))), name = "Bee")
        sub3 <- toVariable(factor(seq_len(nrow(ds))), name = "Mosquito")
        ds$allpets <- addSubvariables(ds$allpets, list(sub2, sub3))
        expect_identical(
            names(subvariables(ds$allpets)),
            c(
                "Cat", "Dog", "Bird", "Turtle", "Gerbil", "Coyote", "Cockroach",
                "Ant", "Bee", "Mosquito"
            )
        )

        sub4 <- toVariable(factor(seq_len(nrow(ds))), name = "Fly")
        ds$Aphid <- factor(seq_len(nrow(ds)))
        ds$allpets <- addSubvariable(ds$allpets, list(sub4, ds$Aphid))
        expect_identical(
            names(subvariables(ds$allpets)),
            c(
                "Cat", "Dog", "Bird", "Turtle", "Gerbil", "Coyote", "Cockroach",
                "Ant", "Bee", "Mosquito", "Fly", "Aphid"
            )
        )
    })
})
