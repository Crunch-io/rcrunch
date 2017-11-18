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
    })

    test_that("Folder extract error handling", {
        expect_null(folders(ds)[["foo"]])
        expect_null(folders(ds)[["Group 1/foo"]])
        expect_error(folders(ds)[["Group 1/foo/bar/baz"]],
            '"Group 1/foo/bar/baz" is an invalid path: foo is not a folder')
        expect_error(folders(ds)[["Group 1/Birth Year/bar/baz"]],
            '"Group 1/Birth Year/bar/baz" is an invalid path: Birth Year is not a folder')
    })

    test_that("Set a folder's name", {
        expect_PATCH(name(folders(ds)[[1]]) <- "First",
            "https://app.crunch.io/api/datasets/1/folders/1/",
            '{"name":"First"}')
    })
    test_that("But top-level folder doesn't have a name and you can't set it", {
        skip("TODO")
    })
    test_that("Set a variable's name", {
        ## Note that this patches the catalog (folder) instead of the entity.
        ## Historical reasons, plus ensuring that name<- on entity and
        ## names<- on catalog do the same thing
        expect_PATCH(name(folders(ds)[["Group 1/Birth Year"]]) <- "Year of birth",
            "https://app.crunch.io/api/datasets/1/folders/1/",
            '{"https://app.crunch.io/api/datasets/1/variables/birthyr/":',
            '{"name":"Year of birth"}}')
    })

    test_that("cd() returns a folder (and not a variable)", {
        expect_identical(cd(ds, "Group 1/Nested"),
            folders(ds)[["Group 1/Nested"]])
    })
    test_that("cd() can operate on a folder too", {
        expect_identical(cd(cd(ds, "Group 1"), "Nested"),
            folders(ds)[["Group 1/Nested"]])
    })
    test_that("cd() errors if the path isn't a folder or doesn't exist", {
        expect_error(cd(ds, "Group 1/Birth Year"),
            '"Group 1/Birth Year" is not a folder')
        expect_error(cd(ds, "Group 1/foo"),
            '"Group 1/foo" is not a folder')
    })
    test_that("cd attempts to create folders if create=TRUE", {
        expect_POST(cd(ds, "Group 1/foo", create=TRUE),
            "https://app.crunch.io/api/datasets/1/folders/1/",
            '{"element":"shoji:catalog","body":{"name":"foo"}}')
        expect_POST(cd(ds, "Group 1/foo/bar", create=TRUE),
            "https://app.crunch.io/api/datasets/1/folders/1/",
            '{"element":"shoji:catalog","body":{"name":"foo"}}')
    })

    ## TODO:
    ## * wire up mv, mkdir, etc.
    ## * names<- on folder
    ## * reorder elements in a folder
    ## * delete a folder
    ## * cd ..
})
