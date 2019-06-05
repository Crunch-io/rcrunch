context("Debugging append")

with_test_authentication({
    whereas("Appending with an exclusion on the incoming dataset", {
        part0 <- createDataset(name = now())
        part1 <- newDatasetFromFixture("apidocs")
        exclusion(part1) <- part1$q1 == "Dog"
        part2 <- newDatasetFromFixture("apidocs")
        exclusion(part2) <- part2$q1 == "Dog"

        part0 <- appendDataset(part0, part1)
        part0 <- appendDataset(part0, part2)

        test_that("Appending applies the exclusion filter of the incoming", {
            expect_identical(
                dim(part0),
                c(nrow(part1) * 2L, ncol(part1))
            )
            expect_equivalent(table(part0$q1)["Dog"], 0)
        })
    })

    whereas("When appending different arrays containing the same subvars", {
        part1 <- mrdf.setup(newDataset(mrdf), name = "CA1")
        part2 <- mrdf.setup(newDataset(mrdf), name = "CA2")

        test_that("The arrays with different aliases have the same subvar aliases", {
            expect_identical(
                aliases(subvariables(part1$CA1)),
                c("mr_1", "mr_2", "mr_3")
            )
            expect_identical(
                aliases(subvariables(part2$CA2)),
                c("mr_1", "mr_2", "mr_3")
            )
        })
        test_that("compareDatasets catches that parent mismatch", {
            comp <- compareDatasets(part1, part2)
            expect_prints(
                summary(comp),
                "Contains subvariables found in other arrays after matching: CA2"
            )
        })
        test_that("The append fails", {
            expect_error(
                appendDataset(part1, part2),
                "Subvariable 'mr_1' cannot be bound to both arrays 'CA2' and 'CA1'."
            )
        })
        part1 <- cleanseBatches(part1)

        test_that(paste0(
            "Can re-alias array variables to make them line up ",
            "**and then drop rows** (and old refs don't reappear)"
        ), {
            alias(part2$CA2) <- "CA1"
            ## This is the critical piece to trigger the error: delete rows after realiasing
            part2 <- dropRows(part2, seq_len(nrow(part2)) == 1)
            out <- appendDataset(part1, part2)
            expect_equal(dim(out), c(2 * nrow(part2) + 1, ncol(part2)))
            expect_identical(
                aliases(subvariables(out$CA1)),
                c("mr_1", "mr_2", "mr_3")
            )
        })
    })

    whereas("Appending arrays with different subvars and derived vars", {
        ds1 <- newDatasetFromFixture("apidocs")
        ds2 <- newDatasetFromFixture("apidocs")
        with_consent(deleteSubvariable(ds1$petloc, "petloc_work"))
        ds1 <- refresh(ds1)
        ds1$comb <- combine(ds1$petloc,
            name = "Comb 1",
            combinations = list(list(name = "Mammals", categories = c("Cat", "Dog")))
        )
        ds1$comb2 <- combine(ds1$petloc,
            name = "Comb 2",
            combinations = list(list(name = "Mammals", categories = c("Cat", "Dog")))
        )
        test_that("The array has one fewer subvars in ds1", {
            expect_identical(aliases(subvariables(ds1$petloc)), "petloc_home")
        })
        test_that("The array has a variable derived from it", {
            expect_length(aliases(subvariables(ds1$comb)), 1)
            expect_length(aliases(subvariables(ds1$comb2)), 1)
            ## Exact alias isn't deterministic
        })
        test_that("In the other dataset, there are both subvars, and no derivation", {
            expect_identical(
                aliases(subvariables(ds2$petloc)),
                c("petloc_home", "petloc_work")
            )
            expect_null(ds2$comb)
        })
        test_that("These append successfully", {
            ## This passes on master but fails here with:
            ## Cannot append subvariables [u'000013', u'000012'] to [u'000012'].
            out <- appendDataset(ds1, ds2)
            expect_true(is.dataset(out))
            expect_true(is.CA(out$comb))
            expect_equal(
                aliases(subvariables(out$petloc)),
                c("petloc_home", "petloc_work")
            )
            expect_length(aliases(subvariables(out$comb)), 2)
        })
    })

    test_that("Derivations with comparison operators can be appended", {
        df1 <- data.frame(foo = rnorm(100), bar = c(1:100))
        ds1 <- newDataset(df1)

        df2 <- data.frame(bar = c(100:1))
        ds2 <- newDataset(df2)

        ds1$new1 <- makeCaseVariable(
            `less than one` = ds1$foo < 1,
            other = "else", name = "new one"
        )
        ds <- appendDataset(ds1, ds2)

        expect_equal(nrow(ds), 200)
        expect_equal(as.vector(ds$foo), c(df1$foo, rep(NA, 100)))
        expect_equal(as.vector(ds$bar), c(df1$bar, df2$bar))
        expect_equal(
            as.character(as.vector(ds$new1)),
            c(
                ifelse(df1$foo < 1, "less than one", "other"),
                rep("other", 100)
            )
        )
    })
})
