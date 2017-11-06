context("Fork and merge")

with_mock_crunch({
    ds1 <- loadDataset("test ds")
    ds2 <- loadDataset("ECON.sav")

    test_that("mergeFork requests", {
        expect_POST(mergeFork(ds1, ds2),
            "https://app.crunch.io/api/datasets/1/actions/",
            '{"element":"shoji:entity",',
            '"body":{"dataset":"https://app.crunch.io/api/datasets/3/",',
            '"autorollback":true,"force":false}}')
        expect_POST(mergeFork(ds1, ds2, autorollback=FALSE),
            "https://app.crunch.io/api/datasets/1/actions/",
            '{"element":"shoji:entity",',
            '"body":{"dataset":"https://app.crunch.io/api/datasets/3/",',
            '"autorollback":false,"force":false}}')
        expect_POST(with(consent(), mergeFork(ds1, ds2, force=TRUE)),
            "https://app.crunch.io/api/datasets/1/actions/",
            '{"element":"shoji:entity",',
            '"body":{"dataset":"https://app.crunch.io/api/datasets/3/",',
            '"autorollback":true,"force":true}}')
    })


    test_that('"Force" merge requires confirmation', {
        expect_error(mergeFork(ds1, ds2, force=TRUE),
            "Must confirm force merge")
    })
})

with_test_authentication({
    ds <- newDataset(df)
    test_that("Fork catalog exists", {
        expect_is(forks(ds), "ForkCatalog")
        expect_length(forks(ds), 0)
    })

    f1 <- forkDataset(ds)
    f1.name <- paste("Fork of", name(ds))
    test_that("Can create a new fork", {
        expect_true(is.dataset(f1))
        expect_identical(name(f1), f1.name)
        expect_identical(names(forks(ds)), f1.name)
        expect_true(is.published(f1))
    })

    exclusion(f1) <- f1$v3 < 11
    f2 <- forkDataset(f1, "Fork yeah!", draft=TRUE)
    test_that("Editing values of data in a new fork doesn't fail", {
        f2$v1[is.na(f2$v1)] <- 42
        expect_equal(as.numeric(table(is.na(f2$v1))["TRUE"]), 0)
    })
    test_that("Can create a fork with a given name (forking from a fork)", {
        expect_true(is.dataset(f2))
        expect_identical(name(f2), "Fork yeah!")
        expect_true("Fork yeah!" %in% names(forks(f1)))
        expect_false(is.published(f2))
    })

    test_that("Forking preserves exclusion filters", {
        expect_output(exclusion(f1), "v3 < 11")
        expect_output(exclusion(f2), "v3 < 11")
        expect_identical(nrow(f2), 17L)
        expect_identical(dim(f1), dim(f2))
    })

    f3 <- forkDataset(ds, draft=FALSE)
    f3.name <- paste("Fork #2 of", name(ds))
    test_that("Creating forks autonames with a fork number", {
        expect_true(is.dataset(f3))
        expect_identical(name(f3), f3.name)
        expect_true(setequal(names(forks(ds)),
            c(f1.name, f3.name)))
        expect_true(is.published(f3))
    })

    with_consent({
        delete(f2)
        delete(f3)
    })
    test_that("If you delete a fork, it disappears from upstream forks catalog", {
        expect_identical(names(refresh(forks(ds))), f1.name)
    })

    ## Make edits to fork #1. cf. test-versioning.R:
    # 0. There's an exclusion, set above.
    # 1. Edit variable metadata
    names(categories(f1$v4))[1:2] <- c("d", "e")
    ## Add a category
    categories(f1$v4) <- c(categories(f1$v4),
        Category(name="F", missing=FALSE, numeric_value=NULL, id=4))
    name(f1$v2) <- "Variable Two"
    description(f1$v3) <- "The third variable in the dataset"

    # 2. Edit dataset metadata
    description(f1) <- "A dataset for testing"

    # 3. Reorder variables
    ordering(f1) <- VariableOrder(VariableGroup("Even", f1[c(2,4,6)]),
        VariableGroup("Odd", f1[c(1,3,5)]))

    # 4. Add non-derived variable
    f1$v8 <- rep(1:5, 4)[4:20]

    # 5. Derive variable
    f1$v7 <- f1$v3 - 6

    # 6. Conditionally edit values of categorical variable
    f1$v4[f1$v8 == 5] <- "F"
    f1$v4[f1$v8 == 4] <- "F"

    # 7. Delete a variable and replace it with one of the same name
    new_vect <- rev(as.vector(f1$v1))
    v1copy <- VariableDefinition(new_vect, name=name(f1$v1), alias=alias(f1$v1))
    test_that("Just asserting that the new var has the same name/alias as old", {
        expect_identical(name(f1$v1), v1copy$name)
        expect_identical(alias(f1$v1), v1copy$alias)
    })
    with_consent(f1$v1 <- NULL)
    f1$v1 <- v1copy

    ## Assert those things
    expect_fork_edits <- function (dataset) {
        expect_output(exclusion(dataset), "v3 < 11")
        expect_identical(dim(dataset), c(17L, 8L))
        expect_identical(names(na.omit(categories(dataset$v4))),
            c("d", "e", "F"))
        expect_equivalent(as.array(crtabs(~ v4, data=dataset)),
            array(c(4, 5, 8), dim=3L, dimnames=list(v4=c("d", "e", "F"))))
        expect_identical(name(dataset$v2), "Variable Two")
        expect_identical(description(dataset$v3),
            "The third variable in the dataset")
        expect_identical(as.vector(dataset$v7), df$v3[4:20] - 6)
        expect_equivalent(as.vector(dataset$v8), rep(1:5, 4)[4:20])
        expect_equivalent(as.vector(dataset$v1), rev(df$v1[4:20]))
        expect_identical(aliases(variables(dataset)),
            paste0("v", c(2,4,6,3,5,8,7,1)))
    }
    test_that("The edits are made to the fork", {
        expect_fork_edits(f1)
        expect_identical(description(f1), "A dataset for testing")
    })

    test_that("The upstream dataset is unaffected by edits to the fork", {
        expect_valid_df_import(ds)
    })

    ## Now merge f1 back to ds
    ds <- mergeFork(ds, f1)
    test_that("The edits made to the fork are now upstream", {
        expect_fork_edits(ds)
    })
    test_that("Certain changes don't merge", {
        expect_identical(description(ds), "")
    })

    whereas("Merging from parent to fork", {
        parent <- newDataset(df)
        child <- forkDataset(parent)

        names(categories(parent$v4))[1:2] <- c("d", "e")
        with_consent(parent$v5 <- NULL)

        test_that("Before merging from parent", {
            expect_identical(names(categories(parent$v4))[1:2], c("d", "e"))
            expect_identical(names(categories(child$v4))[1:2], c("B", "C"))
            expect_null(parent$v5)
            expect_true(is.Datetime(child$v5))
        })

        test_that("After merging from parent", {
            child <- mergeFork(child, parent)
            expect_identical(names(categories(parent$v4))[1:2], c("d", "e"))
            expect_identical(names(categories(child$v4))[1:2], c("d", "e"))
            expect_null(parent$v5)
            expect_null(child$v5)
        })
    })
})
