context("Deriving array variables")

with_test_authentication({
    ds <- newDatasetFromFixture("apidocs")

    ## Reference values:
    plh.values <- as.vector(ds$petloc$petloc_home)
    plw.values <- as.vector(ds$petloc$petloc_work)
    q1.values <- as.vector(ds$q1)
    plh.counts <- table(ds$petloc$petloc_home)
    # petloc_home
    #  Cat  Dog Bird
    #    5    3    3
    plw.counts <- table(ds$petloc$petloc_work)
    # petloc_work
    #  Cat  Dog Bird
    #    6    4    6

    vd <- deriveArray(list(ds$q1, ds$petloc$petloc_home), name="Derived pets")
    test_that("deriveArray returns a VarDef", {
        expect_is(vd, "VariableDefinition")
    })
    ds$derivedarray <- vd
    test_that("Sending a derived array vardef creates a derived array", {
        expect_true(is.CA(ds$derivedarray))
        expect_identical(names(subvariables(ds$derivedarray)), c("Pet", "Home"))
        expect_identical(names(categories(ds$derivedarray)),
            c("Cat", "Dog", "Bird", "Skipped", "Not Asked"))
        expect_identical(as.vector(ds$derivedarray[[1]]), q1.values)
    })

    test_that("Can edit metadata of the derived array, and parents are unaffected", {
        aliases(subvariables(ds$derivedarray)) <- c("dsub1", "dsub2")
        expect_identical(aliases(subvariables(ds$derivedarray)),
            c("dsub1", "dsub2"))
        names(categories(ds$derivedarray))[1:2] <- c("one", "two")
        expect_identical(names(categories(ds$derivedarray)),
            c("one", "two", "Bird", "Skipped", "Not Asked"))
        ds <- refresh(ds)
        expect_identical(aliases(subvariables(ds$derivedarray)),
            c("dsub1", "dsub2"))
        expect_identical(names(categories(ds$derivedarray)),
            c("one", "two", "Bird", "Skipped", "Not Asked"))
        ## Parent unaffected
        expect_true(is.Categorical(ds$q1))
        expect_identical(names(categories(ds$q1)),
            c("Cat", "Dog", "Bird", "Skipped", "Not Asked"))
    })

    ## Deep copy array, rename it, etc.
    pl2 <- copy(ds$petloc, deep=TRUE, name="Other pet loc")
    pl2$subvariables[[1]]$alias <- "pl2_a"
    pl2$subvariables[[2]]$alias <- "pl2_b"
    ds$petloc2 <- pl2

    ## Make a "flipped" version of that and the original
    ds$petloc_a <- deriveArray(list(ds$petloc2$pl2_a, ds$petloc$petloc_home),
        name="Pet Location: Home", subreferences=list(list(name="Copy", alias="pla_copy"),
        list(name="Original", alias="pla_orig")))
    ds$petloc_b <- deriveArray(list(ds$petloc2$pl2_b, ds$petloc$petloc_work),
        name="Pet Location: Work", subreferences=list(list(name="Copy", alias="plb_copy"),
        list(name="Original", alias="plb_orig")))

    test_that("Deriving arrays with subreferences specified", {
        expect_identical(aliases(subvariables(ds$petloc_a)),
            c("pla_copy", "pla_orig"))
        expect_identical(names(subvariables(ds$petloc_a)),
            c("Copy", "Original"))
    })

    ## Make derived array of derived array
    ds$metapetloc <- deriveArray(list(ds$petloc$petloc_home, ds$petloc_a$pla_copy,
        ds$petloc_b$plb_copy, ds$derivedarray$dsub2),
        name="Derived from derived and real")
    test_that("Derived arrays of derived array subvariables with different categories", {
        expect_identical(names(subvariables(ds$metapetloc)),
            c("Home", "Copy", "Copy", "Home"))
        expect_identical(names(categories(ds$metapetloc)),
            c("Cat", "Dog", "Bird", "Skipped", "Not Asked", "one", "two"))

        t1 <- table(ds$metapetloc[[1]])
        expect_identical(names(t1), c("Cat", "Dog", "Bird", "one", "two"))
        expect_equal(as.numeric(t1), c(5, 3, 3, 0, 0))
        expect_identical(as.numeric(table(ds$metapetloc[[2]])), c(5, 3, 3, 0, 0))
        expect_identical(as.numeric(table(ds$metapetloc[[3]])), c(6, 4, 6, 0, 0))
        ## Note the shift: this variable has categories "one" and "two"
        expect_identical(as.numeric(table(ds$metapetloc[[4]])), c(0, 0, 3, 5, 3))
    })

    ## Append to that dataset, make sure all arrays update appropriately
    part2 <- newDatasetFromFixture("apidocs")
    ds <- appendDataset(ds, part2)
    test_that("When appending, derived arrays get data (or missing, as appropriate)", {
        ## No Data is added to the categories here
        expect_identical(names(categories(ds$metapetloc)),
            c("Cat", "Dog", "Bird", "Skipped", "Not Asked", "one", "two", "No Data"))
        ## metapetloc: subvar 1 is derived from petloc$petloc_home
        expect_identical(as.numeric(table(ds$metapetloc[[1]])),
            2 * c(5, 3, 3, 0, 0))
        ## subvar 2 is from petloc_a$pla_1, which points to petloc2, a deep copy
        expect_identical(as.numeric(table(ds$metapetloc[[2]])),
            c(5, 3, 3, 0, 0))
        ## likewise for subvar 3
        expect_identical(as.numeric(table(ds$metapetloc[[3]])),
            c(6, 4, 6, 0, 0))
        ## 4 comes from derivedarray$dsub2, which comes from petloc$petloc_home,
        ## but with different category names
        # print(as.vector(ds$metapetloc[[4]]))
        ## This assertion fails. The new data comes in as Cat Dog and not one two
        expect_identical(as.numeric(table(ds$metapetloc[[4]])),
            2 * c(0, 0, 3, 5, 3))
    })
    test_that("The array we edited in the beginning still has its edits", {
        expect_identical(aliases(subvariables(ds$derivedarray)),
            c("dsub1", "dsub2"))
        expect_identical(names(categories(ds$derivedarray)),
            c("one", "two", "Bird", "Skipped", "Not Asked"))
        ## TODO: assert its values too
    })

    ## TODO: more things (filter entities, drop rows, etc., append where cats aren't the same; edit values of derived array?)
})
