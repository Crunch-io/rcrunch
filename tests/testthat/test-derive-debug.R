context("Derived array variables maintain subvar links")

# nolint start
# derived ds instantaitor
new_ds_with_derived_array <- function() {
    ds <- newDatasetFromFixture("apidocs")
    ds$derivedarray <- deriveArray(
        subvariables = subvariables(ds$petloc),
        name = "Derived pets"
    )
    return(ds)
}
# nolint end

with_test_authentication({
    ds <- new_ds_with_derived_array()

    test_that("Sending a derived array vardef creates a derived array", {
        expect_true(is.derived(ds$derivedarray))
        expect_equivalent(as.vector(ds$derivedarray), as.vector(ds$petloc))
        expect_equivalent(
            as.vector(ds$derivedarray, mode = "id"),
            as.vector(ds$petloc, mode = "id")
        )
    })

    test_that("changing a value in the first subvar carries", {
        ds$petloc$petloc_home[ds$petloc$petloc_home == "Dog"] <- "Cat"

        expect_true(is.derived(ds$derivedarray))
        expect_equivalent(as.vector(ds$derivedarray), as.vector(ds$petloc))
        expect_equivalent(
            as.vector(ds$derivedarray, mode = "id"),
            as.vector(ds$petloc, mode = "id")
        )
    })

    test_that("changing a value in the second subvar carries", {
        ds$petloc$petloc_work[ds$petloc$petloc_work == "Dog"] <- "Cat"

        expect_true(is.derived(ds$derivedarray))
        expect_equivalent(as.vector(ds$derivedarray), as.vector(ds$petloc))
        expect_equivalent(
            as.vector(ds$derivedarray, mode = "id"),
            as.vector(ds$petloc, mode = "id")
        )
    })

    # reinstantiate the dataset so prior failures don't cloud current tests
    ds <- new_ds_with_derived_array()

    test_that("NAing a value carries", {
        ds$petloc$petloc_home[ds$petloc$petloc_home == "Bird"] <- NA

        expect_true(is.derived(ds$derivedarray))
        expect_equivalent(as.vector(ds$derivedarray), as.vector(ds$petloc))
        expect_equivalent(
            as.vector(ds$derivedarray, mode = "id"),
            as.vector(ds$petloc, mode = "id")
        )
    })

    # reinstantiate the dataset so prior failures don't cloud current tests
    ds <- new_ds_with_derived_array()

    test_that("changing category names in metadata carries", {
        existing <- names(categories(ds$petloc))
        existing[1] <- "Kat"
        existing[2] <- "Dogz"
        names(categories(ds$petloc)) <- existing
        ds <- refresh(ds) # must refresh to update the derived variable's metadata

        expect_true(is.derived(ds$derivedarray))
        expect_equivalent(
            categories(ds$derivedarray),
            categories(ds$petloc)
        )
        expect_equivalent(
            categories(ds$derivedarray$`petloc_work__1`),
            categories(ds$petloc$petloc_work)
        )

        # checking the petloc_work subvar since if the above tests failed,
        # we know that petloc_home is broken
        expect_equivalent(
            as.vector(ds$derivedarray$`petloc_work__1`),
            as.vector(ds$petloc$petloc_work)
        )
        expect_equivalent(
            as.vector(ds$derivedarray$`petloc_work__1`, mode = "id"),
            as.vector(ds$petloc$petloc_work, mode = "id")
        )
    })

    # change category ids
    ds$petloc <- changeCategoryID(ds$petloc, 1, 10)
    ds <- refresh(ds) # must refresh to update the derived variable's metadata

    test_that("changing cat ids (values+metadata) metadata", {
        expect_true(is.derived(ds$derivedarray))
        expect_equivalent(
            categories(ds$derivedarray),
            categories(ds$petloc)
        )
        expect_equivalent(
            categories(ds$derivedarray$`petloc_work__1`),
            categories(ds$petloc$petloc_work)
        )
        expect_equivalent(
            categories(ds$derivedarray$`petloc_work__1`),
            categories(ds$petloc$petloc_work)
        )
    })

    test_that("changing cat ids (values+metadata) first subvar", {
        # check the first subvar
        expect_equivalent(
            as.vector(ds$derivedarray$`petloc_home__1`),
            as.vector(ds$petloc$petloc_home)
        )
        expect_equivalent(
            as.vector(ds$derivedarray$`petloc_home__1`, mode = "id"),
            as.vector(ds$petloc$petloc_home, mode = "id")
        )
    })

    test_that("changing cat ids (values+metadata) second subvar", {
        # check the second subvar
        expect_equivalent(
            as.vector(ds$derivedarray$`petloc_work__1`),
            as.vector(ds$petloc$petloc_work)
        )
        expect_equivalent(
            as.vector(ds$derivedarray$`petloc_work__1`, mode = "id"),
            as.vector(ds$petloc$petloc_work, mode = "id")
        )
    })

    test_that("changing cat ids (values+metadata) whole array", {
        # check the whole array
        expect_equivalent(
            as.vector(ds$derivedarray),
            as.vector(ds$petloc)
        )
        expect_equivalent(
            as.vector(ds$derivedarray, mode = "id"),
            as.vector(ds$petloc, mode = "id")
        )
    })

    # Test derive from categorical arrays that are stored as sparse categorical
    #
    # Make a factor that is overwhelmingly NA, but with some combos we want to
    # collapse. Confirmed that this ratio is stored as sparse categorical, but
    # if the definitions for what counts as sparse change, this might need to be
    # changed to maintain coverage
    # fac <- factor(
    #     c(rep("A", 6), rep("B", 5), rep("C", 4),
    #       rep("a", 3), rep("b", 2), rep("c", 1), rep(NA, 979))
    # )
    # first <- sample(fac, 1000)
    # second <- sample(fac, 1000)
    # df <- data.frame(
    #     first = first,
    #     second = second,
    #     first_copy = first,
    #     second_copy = second
    # )
    # # need to change categories to IDs, and then remove NAs
    # write.csv(df, "tests/testthat/dataset-fixtures/sparse_ca.csv", row.names = FALSE)

    # we need to create with metadata to ensure that the categorical array is
    # stored as sparse categorical (if we use bind, then we have to figure out
    # how to trigger a cleanup which is not exposed to the API)
    ds <- createWithMetadataAndFile(
        fromJSON(
            file.path(test_path("dataset-fixtures", "sparse_ca.json")),
            simplifyVector = FALSE
        ),
        test_path(file.path("dataset-fixtures", "sparse_ca.csv"))
    )

    test_that("combine on categorical array stored as sparse returns correct values", {
        # the first categorical array is the same as the copies
        first_copy_vals <- as.vector(ds$first_copy)
        second_copy_vals <- as.vector(ds$second_copy)
        expect_equal(as.vector(ds$cat_array$first), first_copy_vals)
        expect_equal(as.vector(ds$cat_array$second), second_copy_vals)

        # make our combined variable
        ds$ca_combined <- combine(
            ds$cat_array,
            combinations = list(
                list(
                    name = "A",
                    categories = c("A", "a")
                ),
                list(
                    name = "B",
                    categories = c("B", "b")
                ),
                list(
                    name = "C",
                    categories = c("C", "c")
                )
            )
        )

        # combine the values on the vector to compare with the combined variable
        levels(first_copy_vals) <- c("A", "B", "C", "A", "B", "C")
        levels(second_copy_vals) <- c("A", "B", "C", "A", "B", "C")

        expect_equal(as.vector(ds$ca_combined$`first__1`), first_copy_vals)
        expect_equal(as.vector(ds$ca_combined$`second__1`), second_copy_vals)

        # and this might be clearer in a cube of the first subvar and the
        # first_copy, this test is testing the same thing as above, with a cube
        #
        # we expect:
        # first_copy
        # first__1 A B C a b c
        #      A 6 0 0 3 0 0
        #      B 0 5 0 0 2 0
        #      C 0 0 4 0 0 1
        #
        # we get:
        # first_copy
        # first__1 A B C a b c
        #      A 6 5 4 3 0 0
        #      B 0 0 0 0 2 0
        #      C 0 0 0 0 0 1
        dims <- list(
            `first__1` = c("A", "B", "C"),
            first_copy = c("A", "B", "C", "a", "b", "c")
        )

        expect_equivalent(
            as.array(crtabs(~ ca_combined[["first__1"]] + first_copy, ds)),
            cubify(
                6, 0, 0, 3, 0, 0,
                0, 5, 0, 0, 2, 0,
                0, 0, 4, 0, 0, 1,
                dims = dims
            )
        )
    })
})
