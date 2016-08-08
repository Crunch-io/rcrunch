context("Debugging append")

with_test_authentication({
    test_that("Appending applies the exclusion filter of the incoming dataset", {
        part0 <- createDataset(name=now())
        part1 <- newDatasetFromFixture("apidocs")
        exclusion(part1) <- part1$q1 == "Dog"
        part2 <- newDatasetFromFixture("apidocs")
        exclusion(part2) <- part2$q1 == "Dog"

        part0 <- appendDataset(part0, part1)
        part0 <- appendDataset(part0, part2)
        expect_identical(dim(part0),
            c(nrow(part1)*2L, ncol(part1)))
        expect_equivalent(table(part0$q1)["Dog"], 0)
    })

    test_that("Datasets with more rows append (sparseness test)", {
        sparse1 <- newDataset(data.frame(A=factor(c("A", "B")), B=1:1000))
        sparse2 <- newDataset(data.frame(B=1:1000, C=factor(c("C", "D"))))
        out <- appendDataset(sparse1, sparse2)
        expect_identical(mean(out$B), 1001/2)
        expect_length(as.vector(out$C), 2000)
        expect_identical(as.vector(out$C),
            factor(c(rep(NA, 1000), rep(c("C", "D"), 500))))
    })

    test_that("Can re-alias array variables to make them line up (and old refs don't reappear)", {
        part1 <- mrdf.setup(newDataset(mrdf), name="CA1")
        part2 <- mrdf.setup(newDataset(mrdf), name="CA2")
        expect_identical(aliases(subvariables(part1$CA1)),
            aliases(subvariables(part2$CA2)))
        alias(part2$CA2) <- "CA"
        alias(part1$CA1) <- "CA"
        out <- appendDataset(part1, part2)
        expect_equal(dim(out), c(2*nrow(part2), ncol(part2)))
    })
})
