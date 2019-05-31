context("Deriving array variables")

with_mock_crunch({
    ds <- loadDataset("test ds")
    test_that("deriveArray works with MR", {
        expect_POST(
            ds$derived_mr <- deriveArray(list(ds$gender),
                selections = list("Female"),
                name = "derivedMR"
            ),
            "https://app.crunch.io/api/datasets/1/variables/",
            '{"derivation":{"function":"select_categories","args":',
            '[{"function":"array","args":[{"function":"select","args":',
            '[{"map":{"1":{"variable":"https://app.crunch.io/api/datasets',
            '/1/variables/gender/"}}},{"value":["1"]}]}]},{"value":',
            '["Female"]}]},"name":"derivedMR","alias":"derived_mr"}'
        )
    })
})


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

    vd <- deriveArray(list(ds$q1, ds$petloc$petloc_home), name = "Derived pets")
    test_that("deriveArray returns a VarDef", {
        expect_is(vd, "VariableDefinition")
    })
    ds$derivedarray <- vd
    test_that("Sending a derived array vardef creates a derived array", {
        expect_true(is.CA(ds$derivedarray))
        expect_identical(names(subvariables(ds$derivedarray)), c("Pet", "Home"))
        expect_identical_temp_nodata(
            names(categories(ds$derivedarray)),
            c("Cat", "Dog", "Bird", "Skipped", "Not Asked", "No Data")
        )
        expect_identical(as.vector(ds$derivedarray[[1]]), q1.values)
    })

    ds$derivedmr <- deriveArray(list(ds$q1, ds$petloc$petloc_home), selections = "Dog", name = "Derived pets MR") # cat dog has id 2
    test_that("deriveArray can also derive MRs", {
        expect_true(is.MR(ds$derivedmr))
        expect_identical(names(subvariables(ds$derivedmr)), c("Pet", "Home"))
        expect_identical_temp_nodata(
            names(categories(ds$derivedmr)),
            c("Cat", "Dog", "Bird", "Skipped", "Not Asked", "No Data")
        )
        expect_identical(as.vector(ds$derivedmr[[1]]), q1.values)
        expect_true(is.selected(categories(ds$derivedmr)[[2]]))
    })

    test_that("Can edit metadata of the derived array, and parents are unaffected", {
        aliases(subvariables(ds$derivedarray)) <- c("dsub1", "dsub2")
        expect_identical(
            aliases(subvariables(ds$derivedarray)),
            c("dsub1", "dsub2")
        )
        names(categories(ds$derivedarray))[1:2] <- c("one", "two")
        expect_identical_temp_nodata(
            names(categories(ds$derivedarray)),
            c("one", "two", "Bird", "Skipped", "Not Asked", "No Data")
        )
        ds <- refresh(ds)
        expect_identical(
            aliases(subvariables(ds$derivedarray)),
            c("dsub1", "dsub2")
        )
        expect_identical_temp_nodata(
            names(categories(ds$derivedarray)),
            c("one", "two", "Bird", "Skipped", "Not Asked", "No Data")
        )
        expect_equivalent(
            table(ds$derivedarray$dsub2),
            structure(c(5, 3, 3),
                .Dim = 3L,
                .Dimnames = list(dsub2 = c("one", "two", "Bird")),
                class = "table"
            )
        )
        ## Parent unaffected
        expect_true(is.Categorical(ds$q1))
        expect_identical_temp_nodata(
            names(categories(ds$q1)),
            c("Cat", "Dog", "Bird", "Skipped", "Not Asked", "No Data")
        )
    })

    ## Deep copy array, rename it, etc.
    ds$petloc2 <- copy(ds$petloc, deep = TRUE, name = "Other pet loc")
    aliases(subvariables(ds$petloc2)) <- c("pl2_a", "pl2_b")

    ## Make a "flipped" version of that and the original
    ds$petloc_a <- deriveArray(list(ds$petloc2$pl2_a, ds$petloc$petloc_home),
        name = "Pet Location: Home", subreferences = list(
            list(name = "Copy", alias = "pla_copy"),
            list(name = "Original", alias = "pla_orig")
        )
    )
    ds$petloc_b <- deriveArray(list(ds$petloc2$pl2_b, ds$petloc$petloc_work),
        name = "Pet Location: Work", subreferences = list(
            list(name = "Copy", alias = "plb_copy"),
            list(name = "Original", alias = "plb_orig")
        )
    )

    test_that("Deriving arrays with subreferences specified", {
        expect_identical(
            aliases(subvariables(ds$petloc_a)),
            c("pla_copy", "pla_orig")
        )
        expect_identical(
            names(subvariables(ds$petloc_a)),
            c("Copy", "Original")
        )
    })

    ## Make derived array of derived array
    ds$metapetloc <- deriveArray(list(
        ds$petloc$petloc_home, ds$petloc_a$pla_copy,
        ds$petloc_b$plb_copy, ds$derivedarray$dsub2
    ),
    name = "Derived from derived and real"
    )
    test_that("Derived arrays of derived array subvariables with different categories", {
        expect_identical(
            names(subvariables(ds$metapetloc)),
            c("Home", "Copy", "Copy", "Home")
        )
        expect_identical_temp_nodata(
            names(categories(ds$metapetloc)),
            c("Cat", "Dog", "Bird", "Skipped", "Not Asked", "No Data", "one", "two")
        )

        t1 <- table(ds$metapetloc[[1]])
        expect_identical(names(t1), c("Cat", "Dog", "Bird", "one", "two"))
        expect_equal(as.numeric(t1), c(5, 3, 3, 0, 0))
        expect_identical(as.numeric(table(ds$metapetloc[[2]])), c(5, 3, 3, 0, 0))
        expect_identical(as.numeric(table(ds$metapetloc[[3]])), c(6, 4, 6, 0, 0))
        ## Note the shift: this variable has categories "one" and "two"
        expect_identical(as.numeric(table(ds$metapetloc[[4]])), c(0, 0, 3, 5, 3))
    })

    ds$metapet_combined <- combine(ds$metapetloc,
        name = "Metapet combined",
        combinations = list(
            list(name = "Cat", categories = c("Cat", "one")),
            list(name = "Dog", categories = c("Dog", "two"))
        )
    )
    test_that("Combine categories of derived array", {
        expect_identical(
            names(subvariables(ds$metapet_combined)),
            c("Home", "Copy", "Copy", "Home")
        )
        expect_identical_temp_nodata(
            names(categories(ds$metapet_combined)),
            c("Cat", "Dog", "Bird", "Skipped", "Not Asked", "No Data")
        )

        t1 <- table(ds$metapet_combined[[1]])
        expect_identical(names(t1), c("Cat", "Dog", "Bird"))
        expect_equal(as.numeric(t1), c(5, 3, 3))
        expect_identical(as.numeric(table(ds$metapet_combined[[2]])), c(5, 3, 3))
        expect_identical(as.numeric(table(ds$metapet_combined[[3]])), c(6, 4, 6))
        ## Now these are combined back together
        expect_identical(as.numeric(table(ds$metapet_combined[[4]])), c(5, 3, 3))
    })
    ## Append to that dataset, make sure all arrays update appropriately
    part2 <- newDatasetFromFixture("apidocs")
    ds <- appendDataset(ds, part2)
    test_that("When appending, derived arrays get data (or missing, as appropriate)", {
        ## No Data is added to the categories here
        # Replace this `expect_true(identical(new) || identical(old))`
        # construction with `expect_identical(new)` once the "default values"
        # ticket https://www.pivotaltracker.com/story/show/164939686 is released.
        expect_true(
            identical(
                names(categories(ds$metapetloc)),
                c("Cat", "Dog", "Bird", "Skipped", "Not Asked", "one", "two", "No Data")
            ) || identical(
                names(categories(ds$metapetloc)),
                c("Cat", "Dog", "Bird", "Skipped", "Not Asked", "No Data", "one", "two")
            )
        )
        ## metapetloc: subvar 1 is derived from petloc$petloc_home
        expect_identical(
            as.numeric(table(ds$metapetloc[[1]])),
            2 * c(5, 3, 3, 0, 0)
        )
        ## subvar 2 is from petloc_a$pla_1, which points to petloc2, a deep copy
        expect_identical(
            as.numeric(table(ds$metapetloc[[2]])),
            c(5, 3, 3, 0, 0)
        )
        ## likewise for subvar 3
        expect_identical(
            as.numeric(table(ds$metapetloc[[3]])),
            c(6, 4, 6, 0, 0)
        )
        ## 4 comes from derivedarray$dsub2, which comes from petloc$petloc_home,
        ## but with different category names

        ## This assertion fails. The new data comes in as Cat Dog and not one two
        # print(table(ds$metapetloc[[4]]))
        # dsub2#
        #  Cat  Dog Bird  one  two
        #    5    3    6    5    3
        expect_identical(
            as.numeric(table(ds$metapetloc[[4]])),
            2 * c(0, 0, 3, 5, 3)
        )
    })
    test_that("Combined categories on top of derived array also gets the right data", {
        expect_identical(
            names(categories(ds$metapet_combined)),
            c("Cat", "Dog", "Bird", "Skipped", "Not Asked", "No Data")
        )
        expect_identical(
            as.numeric(table(ds$metapet_combined[[1]])),
            2 * c(5, 3, 3)
        )
        expect_identical(
            as.numeric(table(ds$metapet_combined[[2]])),
            c(5, 3, 3)
        )
        expect_identical(
            as.numeric(table(ds$metapet_combined[[3]])),
            c(6, 4, 6)
        )
        expect_identical(
            as.numeric(table(ds$metapet_combined[[4]])),
            2 * c(5, 3, 3)
        )
    })
    test_that("The array we edited in the beginning still has its edits after appending", {
        expect_identical(
            aliases(subvariables(ds$derivedarray)),
            c("dsub1", "dsub2")
        )
        expect_identical_temp_nodata(
            names(categories(ds$derivedarray)),
            c("one", "two", "Bird", "Skipped", "Not Asked", "No Data")
        )
        ## This is the data we should see in metapetloc[[4]]. It's correct here.
        expect_equivalent(
            table(ds$derivedarray$dsub2),
            structure(c(10, 6, 6),
                .Dim = 3L,
                .Dimnames = list(dsub2 = c("one", "two", "Bird")),
                class = "table"
            )
        )
    })

    ## Append a dataset where the categories in one of the base variables don't quite match
    part3 <- newDatasetFromFixture("apidocs")
    names(categories(part3$petloc))[2] <- "Beaver"
    aliases(subvariables(part3$petloc))[2] <- "petloc_school"
    names(subvariables(part3$petloc))[2] <- "School"
    ds <- appendDataset(ds, part3)

    test_that("petloc is updated appropriately", {
        expect_identical(
            aliases(subvariables(ds$petloc)),
            c("petloc_home", "petloc_work", "petloc_school")
        )
        ## No Data comes in after appending because petloc_work and petloc_school have data gaps
        # Replace this `expect_true(identical(new) || identical(old))`
        # construction with `expect_identical(new)` once the "default values"
        # ticket https://www.pivotaltracker.com/story/show/164939686 is released.
        expect_true(
            identical(
                names(categories(ds$petloc)),
                c("Cat", "Dog", "Bird", "Skipped", "Not Asked", "Beaver", "No Data")
            ) || identical(
                names(categories(ds$petloc)),
                c("Cat", "Dog", "Bird", "Skipped", "Not Asked", "No Data", "Beaver")
            )
        )
    })

    # skip("Derivations of derivations need some work in zz9")
    # test_that("derivedarray gets updated appropriately", {
    #     expect_identical(aliases(subvariables(ds$derivedarray)),
    #         c("dsub1", "dsub2"))
    #     ## No Data and Beaver come in from petloc
    #     expect_identical(names(categories(ds$derivedarray)),
    #         c("one", "two", "Bird", "Skipped", "Not Asked", "Beaver", "No Data"))
    #     # print(table(ds$derivedarray))
    #     # expect_equivalent(table(ds$derivedarray),
    #     #     structure(c(10, 6, 6), .Dim=3L,
    #     #     .Dimnames=list(dsub2=c("one", "two", "Bird")),
    #     #     class="table"))
    # })
    # test_that("petloc_a gets updated appropriately", {
    #     ## petloc_a has Copy and Original, which is petloc_home
    #     expect_identical(aliases(subvariables(ds$petloc_a)),
    #         c("pla_copy", "pla_orig"))
    #     expect_identical(names(subvariables(ds$petloc_a)),
    #         c("Copy", "Original"))
    #     expect_identical(names(categories(ds$petloc_a)),
    #         c("Cat", "Dog", "Bird", "Skipped", "Not Asked", "No Data", "Beaver"))
    #     ## table(that)
    # })
    # test_that("petloc_b gets updated appropriately", {
    #     ## petloc_a has Copy and Original, which is petloc_work. But there is
    #     ## no petloc_work in the new batch
    #     expect_identical(aliases(subvariables(ds$petloc_b)),
    #         c("plb_copy", "plb_orig"))
    #     expect_identical(names(subvariables(ds$petloc_b)),
    #         c("Copy", "Original"))
    #     expect_identical(names(categories(ds$petloc_b)),
    #         c("Cat", "Dog", "Bird", "Skipped", "Not Asked", "No Data", "Beaver"))
    #     ## table(that). all NA for the new batch
    # })
    # test_that("metapetloc gets updated appropriately", {
    #     # petloc$petloc_home, petloc_a$pla_copy, petloc_b$plb_copy, derivedarray$dsub2
    #     expect_identical(names(categories(ds$metapetloc)),
    #         c("Cat", "Dog", "Bird", "Skipped", "Not Asked", "one", "two", "No Data", "Beaver"))
    #     ## metapetloc: subvar 1 is derived from petloc$petloc_home.
    #     ## Beaver instead of Dog in new wave
    #     expect_identical(as.numeric(table(ds$metapetloc[[1]])),
    #         c(15, 6, 9, 0, 0, 3))
    #     ## subvar 2 is from petloc_a$pla_1, which points to petloc2, a deep copy
    #     expect_identical(as.numeric(table(ds$metapetloc[[2]])),
    #         c(5, 3, 3, 0, 0, 0))
    #     ## likewise for subvar 3
    #     expect_identical(as.numeric(table(ds$metapetloc[[3]])),
    #         c(6, 4, 6, 0, 0, 0))
    #     ## 4 comes from derivedarray$dsub2, which comes from petloc$petloc_home,
    #     ## but with different category names

    #     ## This assertion fails. The new data comes in as Cat Dog and not one two
    #     # print(table(ds$metapetloc[[4]]))
    #     expect_identical(as.numeric(table(ds$metapetloc[[4]])),
    #         c(0, 0, 9, 10, 6, 3))
    # })
    # test_that("metapet_combined gets updated appropriately", {
    #     expect_identical(names(categories(ds$metapet_combined)),
    #         c("Cat", "Dog", "Bird", "Skipped", "Not Asked", "No Data", "Beaver"))
    #     expect_identical(as.numeric(table(ds$metapet_combined[[1]])),
    #         c(15, 6, 9, 3))
    #     expect_identical(as.numeric(table(ds$metapet_combined[[2]])),
    #         c(5, 3, 3))
    #     expect_identical(as.numeric(table(ds$metapet_combined[[3]])),
    #         c(6, 4, 6))
    #     expect_identical(as.numeric(table(ds$metapet_combined[[4]])),
    #         c(15, 6, 9, 3))
    # })

    ## TODO: more things (filter entities, drop rows, etc., edit values of base variables; edit values of derived array?)
})
