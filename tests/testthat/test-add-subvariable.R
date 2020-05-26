context("Adding subvariables")

with_mock_crunch({
    ds <- loadDataset("test ds")
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

    ds <- loadDataset("test ds addsubvars to derived")
    test_that("Can add existing variable to derived select array", {
        expect_PATCH(
            ds$xcat <- addSubvariable(ds$xcat, ds["x3"]),
            "https://app.crunch.io/api/datasets/40ccf1/variables/2b72a9/",
            '{"derivation":{"function":"array","args":[{"function":"select","args":[{"map":',
            '{"1":{"variable":"7lgPl0MFNP5PJTxWJEStnk000000"},"2":{"variable":"7lgPl0MFNP5PJTxWJEStnk000001"},', #nolint
            '"3":{"variable":"https://app.crunch.io/api/datasets/40ccf1/variables/7lgPl0MFNP5PJTxWJEStnk000002/"}}}', #nolint
            ',{"value":["1","2","3"]}]}],"references":{"alias":"xcat","name":"x cat"}}}'
        )
    })
    test_that("Can add existing variable to derived select_cat array", {
        expect_PATCH(
            ds$xmr <- addSubvariable(ds$xmr, ds["x3"]),
            "https://app.crunch.io/api/datasets/40ccf1/variables/e0999e/",
            '{"derivation":{"function":"select_categories","args":[{"function":"array","args":[{"function":"select"', # nolint
            ',"args":[{"map":{"1":{"variable":"7lgPl0MFNP5PJTxWJEStnk000000"},',
            '"2":{"variable":"7lgPl0MFNP5PJTxWJEStnk000001"},',
            '"3":{"variable":"https://app.crunch.io/api/datasets/40ccf1/variables/7lgPl0MFNP5PJTxWJEStnk000002/"}}}', #nolint
            ',{"value":["1","2","3"]}]}]},{"value":["Good"]}],"references":{"alias":"xmr","name":"x mr"}}}' # nolint
        )
    })
    test_that("Can add VarDef to derived select array", {
        expect_PATCH(
            ds$xcat <- addSubvariable(ds$xcat, VarDef(as.Categorical(ds$x_text), name = "x text")),
            "https://app.crunch.io/api/datasets/40ccf1/variables/2b72a9/",
            '{"derivation":{"function":"array","args":[{"function":"select","args":[{"map":',
            '{"1":{"variable":"7lgPl0MFNP5PJTxWJEStnk000000"},"2":{"variable":"7lgPl0MFNP5PJTxWJEStnk000001"},', #nolint
            '"3":{"function":"cast","args":[{"variable":',
            '"https://app.crunch.io/api/datasets/40ccf1/variables/7lgPl0MFNP5PJTxWJEStnk000003/"}',
            ',{"value":"categorical"}],"references":{"name":"x text"}}}}', #nolint
            ',{"value":["1","2","3"]}]}],"references":{"alias":"xcat","name":"x cat"}}}'
        )
    })
    test_that("Can add VarDef to derived select_cat array", {
        expect_PATCH(
            ds$xmr <- addSubvariable(ds$xmr, VarDef(as.Categorical(ds$x_text), name = "x text")),
            "https://app.crunch.io/api/datasets/40ccf1/variables/e0999e/",
            '{"derivation":{"function":"select_categories","args":[{"function":"array","args":[{"function":"select"', # nolint
            ',"args":[{"map":{"1":{"variable":"7lgPl0MFNP5PJTxWJEStnk000000"},',
            '"2":{"variable":"7lgPl0MFNP5PJTxWJEStnk000001"},"3":{"function":',
            '"cast","args":[{"variable":"https://app.crunch.io/api/datasets/40ccf1/variables/7lgPl0MFNP5PJTxWJEStnk000003/"},', #nolint
            '{"value":"categorical"}],"references":{"name":"x text"}}}},{"value":["1","2","3"]}]}]}', #nolint
            ',{"value":["Good"]}],"references":{"alias":"xmr","name":"x mr"}}}'
        )
    })
    test_that(
        "Cannot add new categories when adding existing variable to derived select_cat array", {
            expect_error(
                ds$xmr <- addSubvariable(ds$xmr, ds["y"]),
                "Some existing variables have categories not already present in the MR variable"
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

with_test_authentication({
    # adding to derived array
    # TODO: Use existing fixture? Add this to fixtures?
    levels <- c("Good", "Okay", "Bad")
    data <- data.frame(
        x1 = factor(c("Good", "Okay", "Bad", "Good", "Okay"), levels),
        x2 = factor(c("Okay", "Bad", "Good", "Okay", "Okay"), levels),
        x3 = factor(c("Bad", "Good", "Okay", "Good", "Bad"), levels),
        x_text = c("Bad", "Good", "Okay", "Good", "Bad"),
        stringsAsFactors = FALSE
    )

    ds <- newDataset(data, name = "add subvars to derived test")

    ds$xcat <- deriveArray(ds[c('x1', 'x2')], name = "x cat")
    ds$xmr <- deriveArray(ds[c('x1', 'x2')], name = "x mr", selections = "Good")

    test_that("Can add existing variable to select-style categorical.", {
        ds$xcat <- addSubvariable(ds$xcat, ds["x3"])
        expect_equal(as.vector(ds$x3), as.vector(ds$xcat[[3]]))
        expect_equal(names(subvariables(ds$xcat)), c("x1", "x2", "x3"))
    })

    test_that("Can add existing variable to select_cat-style categorical.", {
        ds$xmr <- addSubvariable(ds$xmr, ds["x3"])
        expect_equal(as.vector(ds$x3), as.vector(ds$xmr[[3]]))
        expect_equal(names(subvariables(ds$xmr)), c("x1", "x2", "x3"))
    })

    test_that("Can add vardef variable to select-style categorical.", {
        ds$xcat <- addSubvariable(
            ds$xcat,
            VarDef(
                as.Categorical(ds$x_text),
                name = "xtext",
                description = "xt desc",
                notes = "xt notes"
            )
        )
        expect_equal(as.vector(ds$x_text), as.character(as.vector(ds$xcat[[4]])))
        expect_equal(names(subvariables(ds$xcat)), c("x1", "x2", "x3", "xtext"))
    })

    test_that("Can add vardef variable to select_cat-style categorical.", {
        ds$xmr <- addSubvariable(
            ds$xmr,
            VarDef(
                as.Categorical(ds$x_text),
                name = "xtext",
                description = "xtext desc",
                notes = "xtext notes"
            )
        )
        expect_equal(as.vector(ds$x_text), as.character(as.vector(ds$xmr[[4]])))
        expect_equal(names(subvariables(ds$xmr)), c("x1", "x2", "x3", "xtext"))
    })
})
