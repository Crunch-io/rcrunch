context("Variable folders")

with_mock_crunch({
    ds <- loadDataset("test ds")

    test_that("Can load the root variable folder", {
        expect_is(folders(ds), "VariableFolder")
    })
    test_that("Folder contents are ordered by @graph", {
        expect_identical(names(folders(ds)), c("Group 1", "Group 2"))
    })
    test_that("Other folder methods", {
        expect_identical(types(folders(ds)), c("folder", "folder"))
    })

    test_that("Can [[ from a folder", {
        expect_is(folders(ds)[[1]], "VariableFolder")
        expect_identical(name(folders(ds)[[1]]), "Group 1")
        expect_is(folders(ds)[["Group 2"]], "VariableFolder")
        expect_identical(name(folders(ds)[["Group 2"]]), "Group 2")
    })

    g1 <- folders(ds)[[1]]
    test_that("Folder with heterogeneous types", {
        expect_identical(names(g1), c("Birth Year", "Nested", "Text variable ftw"))
        expect_identical(types(g1), c("numeric", "folder", "text"))
        expect_identical(aliases(g1), c("birthyr", NA, "textVar"))
        # Extracting by alias
        expect_identical(
            names(g1[c("birthyr", "textVar")]),
            c("Birth Year", "Text variable ftw")
        )
    })
    test_that("variables() method on folders", {
        expect_identical(aliases(variables(g1)), c("birthyr", "textVar"))
    })
    test_that("Get folder from a folder (via $)", {
        expect_is(g1$Nested, "VariableFolder")
        expect_identical(name(g1$Nested), "Nested")
    })
    test_that("Get a variable from a folder", {
        expect_is(g1[[1]], "NumericVariable")
        expect_identical(name(g1[[1]]), "Birth Year")
        ## Not identical to ds$birthyr bc the catalog tuples have different
        ## contents, but they work the same
        expect_identical(self(g1[[1]]), self(ds$birthyr))
        expect_identical(summary(g1[[1]]), summary(ds$birthyr))
    })

    test_that("Extract from a folder by path", {
        expect_is(folders(ds)[["Group 1/Nested"]], "VariableFolder")
        expect_identical(folders(ds)[["Group 1/Nested"]], g1$Nested)
        expect_is(folders(ds)[["Group 1/Birth Year"]], "NumericVariable")
        expect_identical(name(folders(ds)[["Group 1/Birth Year"]]), "Birth Year")
        expect_is(folders(ds)[["Group 1/birthyr"]], "NumericVariable")
    })

    test_that("Folder extract error handling", {
        expect_null(folders(ds)[["foo"]])
        expect_null(folders(ds)[["Group 1/foo"]])
        expect_error(
            folders(ds)[["Group 1/foo/bar/baz"]],
            '"Group 1/foo/bar/baz" is an invalid path: foo is not a folder'
        )
        expect_error(
            folders(ds)[["Group 1/Birth Year/bar/baz"]],
            '"Group 1/Birth Year/bar/baz" is an invalid path: Birth Year is not a folder'
        )
        expect_error(
            folders(ds)[["Group 1/birthyr/bar/baz"]],
            '"Group 1/birthyr/bar/baz" is an invalid path: birthyr is not a folder'
        )
    })

    test_that("Set a folder's name", {
        expect_PATCH(
            name(folders(ds)[[1]]) <- "First",
            "https://app.crunch.io/api/datasets/1/folders/1/",
            '{"name":"First"}'
        )
    })
    test_that("But top-level folder doesn't have a name and you can't set it", {
        skip("TODO")
    })
    test_that("Set names of objects inside a folder", {
        expect_PATCH(
            names(folders(ds)[[1]]) <- c("Year of Birth", "A folder in a folder", "Plain text"),
            "https://app.crunch.io/api/datasets/1/folders/1/",
            '{"element":"shoji:catalog","index":',
            '{"https://app.crunch.io/api/datasets/1/variables/birthyr/":',
            '{"name":"Year of Birth"},',
            '"https://app.crunch.io/api/datasets/1/folders/3/":',
            '{"name":"A folder in a folder"},',
            '"https://app.crunch.io/api/datasets/1/variables/textVar/":',
            '{"name":"Plain text"}}}'
        )
    })
    test_that("Set a variable's name inside a folder", {
        ## Note that this patches the catalog (folder) instead of the entity.
        ## Historical reasons, plus ensuring that name<- on entity and
        ## names<- on catalog do the same thing
        expect_PATCH(
            name(folders(ds)[["Group 1/Birth Year"]]) <- "Year of birth",
            "https://app.crunch.io/api/datasets/1/folders/1/",
            '{"https://app.crunch.io/api/datasets/1/variables/birthyr/":',
            '{"name":"Year of birth"}}'
        )
    })

    test_that("folder() finds the parent folder", {
        expect_identical(ds %>% cd("Group 1") %>% folder(), folders(ds))
        expect_identical(folder(cd(ds, "Group 1/Nested")), cd(ds, "Group 1"))
        expect_identical(
            folder(folders(ds)[["Group 1/Birth Year"]]),
            cd(ds, "Group 1")
        )
        expect_null(folder(folders(ds)))
        expect_error(folder("string"), "No folder for object of class character")
    })

    test_that("rootFolder() finds the top level", {
        expect_identical(rootFolder(folders(ds)), folders(ds))
        expect_identical(rootFolder(folders(ds)[["Group 1/Nested"]]), folders(ds))
        expect_identical(rootFolder(ds$birthyr), folders(ds))
    })

    test_that("delete folder", {
        expect_error(
            delete(folders(ds)[["Group 1/Nested"]]),
            "Must confirm deleting folder"
        )
        with_consent({
            expect_DELETE(
                delete(folders(ds)[["Group 1/Nested"]]),
                "https://app.crunch.io/api/datasets/1/folders/3/"
            )
        })
        expect_error(
            delete(folders(ds)),
            "Cannot delete root folder"
        )
    })

    test_that("path()", {
        expect_identical(path(folders(ds)[["Group 1/Nested"]]), "/Group 1/Nested") # nolint
        expect_identical(path(ds$birthyr), "/Group 1/Birth Year") # nolint
        expect_identical(path(folders(ds)), "/")
    })

    with(temp.option(crayon.enabled = FALSE), {
        test_that("print folders: basic", {
            ## Coloring aside, the default print method should look like you
            ## printed the vector of names (plus the path printed above)
            expect_output(
                print(folders(ds)),
                capture.output(print(names(folders(ds)))),
                fixed = TRUE
            )
        })
        test_that("An empty folder doesn't error on printing", {
            expect_output(
                print(VariableFolder()),
                "folder(0)",
                fixed = TRUE
            )
        })
        test_that("If there are names longer than 'width', it still prints", {
            skip_on_jenkins("Unskip when testthat is next updated on jenkins")
            alphabet <- paste(letters, collapse = "")
            expect_output(
                colored_print(alphabet),
                '[1] "abcdefghijklmnopqrstuvwxyz"',
                fixed = TRUE,
                width = 10
            )
        })
        test_that("Folder tree printing", {
            ## These are obfuscated because of archaic restrictions on UTF-8
            skip_on_cran()
            source("print-folders.R", local = TRUE)
        })
    })

    test_that("copyFolders returns the target dataset with the order applied", {
        ds_again <- loadDataset("test ds")
        expect_silent(new_order <- copyFolders(ds, ds_again))
        expect_is(new_order, "CrunchDataset")
        expect_identical(entities(ordering(ds)), entities(ordering(new_order)))
    })

    test_that("copyFolders input validation", {
        expect_error(
            copyFolders(ds, "foo"),
            "Both source and target must be Crunch datasets."
        )
    })
})


with_test_authentication({
    ds <- createDataset(name = now())
    test_that("folders are enabled by default in the tests", {
        expect_true(settings(ds)$variable_folders)
    })

    ds <- newDataset(df)
    test_that("copyFolders copies across datasets with simple order", {
        ds_fork <- forkDataset(ds)
        old_order <- capture_output_lines(folders(ds_fork) %>% print(depth = 10))
        ds %>% cd("/") %>% setOrder(c("v1", "v2", "v5", "v6", "v3", "v4"))
        ds <- refresh(ds)

        # test that ds_fork has the old order still
        expect_identical(
            capture_output_lines(folders(ds_fork) %>% print(depth = 10)),
            old_order
        )
        expect_false(identical(
            capture_output_lines(folders(ds_fork) %>% print(depth = 10)),
            capture_output_lines(folders(ds) %>% print(depth = 10))
        ))

        # copy order, and check that ds_fork has the new order.
        ds_fork <- copyFolders(ds, ds_fork)

        expect_identical(
            capture_output_lines(folders(ds_fork) %>% print(depth = 10)),
            capture_output_lines(folders(ds) %>% print(depth = 10))
        )
    })

    test_that("copyFolders copies across datasets with simple(-ish) order (and one nesting)", {
        ds_fork <- forkDataset(ds)
        old_order <- capture_output_lines(folders(ds_fork) %>% print(depth = 10))
        ds %>% cd ("/") %>% mkdir("Group A") %>% mv(c("v4", "v3"), "Group A")

        # test that ds_fork has the old order still
        expect_identical(
            capture_output_lines(folders(ds_fork) %>% print(depth = 10)),
            old_order
        )
        expect_false(identical(
            capture_output_lines(folders(ds) %>% print(depth = 10)),
            capture_output_lines(folders(ds_fork) %>% print(depth = 10))
        ))

        # copy order, and check that ds_fork has the new order.
        ds_fork <- copyFolders(ds, ds_fork)
        expect_identical(
            capture_output(folders(ds_fork) %>% print(depth = 10)),
            capture_output(folders(ds) %>% print(depth = 10))
        )
    })


    test_that("copyFolders copies across datasets with nested hierarchical order", {
        ds_fork <- forkDataset(ds)
        old_order <- capture_output_lines(folders(ds_fork) %>% print(depth = 10))

        # reset order
        ds %>% mv(aliases(allVariables(ds)), "/")

        ds %>%
            mkdir("Group 1") %>%
            mv(c("v1", "v2"), "Group 1") %>%
            mkdir("Group 1/Group 1.5") %>%
            mv(c("v5", "v6"), "/Group 1/Group 1.5") %>% # nolint
            mkdir("Group 2") %>%
            mv(c("v4", "v3"), "/Group 2") # nolint

        # test that ds_fork has the old order still
        expect_identical(
            capture_output_lines(folders(ds_fork) %>% print(depth = 10)),
            old_order
        )
        expect_false(identical(
            capture_output_lines(folders(ds) %>% print(depth = 10)),
            capture_output_lines(folders(ds_fork) %>% print(depth = 10))
        ))

        # copy order, and check that ds_fork has the new order.
        ds_fork <- copyFolders(ds, ds_fork)
        expect_identical(
            capture_output_lines(folders(ds_fork) %>% print(depth = 10)),
            capture_output_lines(folders(ds) %>% print(depth = 10))
        )
    })

    test_that("copyFolders copies across disparate datasets", {
        # setup an alternative dataset that has some overlap with ds
        df_alt <- df
        df_alt$v12 <- df_alt$v1
        df_alt$v1 <- NULL
        df_alt$v2 <- NULL
        df_alt$new_var <- 1
        df_alt$new_var2 <- letters[20:1]
        ds_alt <- newDataset(df_alt)

        old_order <- capture_output_lines(folders(ds_alt) %>% print(depth = 10))

        # reset order
        with_consent(
            ds %>%
                mv(aliases(allVariables(ds)), "/") %>%
                rmdir(c("Group 1")) %>%
                rmdir(c("Group 2")) %>%
                rmdir(c("Group A"))
        )


        ds %>% mkdir("Group A") %>% mv(c("v4", "v3"), "Group A")

        # test that ds_alt has the old order still
        expect_identical(
            capture_output_lines(folders(ds_alt) %>% print(depth = 10)),
            old_order
        )

        expect_false(identical(
            capture_output_lines(folders(ds_alt) %>% print(depth = 10))[-c(4, 5, 6)],
            capture_output_lines(folders(ds) %>% print(depth = 10))[-c(2, 3)]
        ))

        # copy order, and check that ds_alt has the new order.
        ds_alt <- copyFolders(ds, ds_alt)
        expect_identical(
            # ignore lines 4, 5, and 6 because they are vars in ds_alt that are
            # not in ds
            capture_output_lines(folders(ds_alt) %>% print(depth = 10))[-c(4, 5, 6)],
            # ignore lines 3 and 2 because they are vars in ds that are
            # not in ds_alt
            capture_output_lines(folders(ds) %>% print(depth = 10))[-c(2, 3)]
        )
    })
})
