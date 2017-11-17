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
        ## Not identical to ds$birthyr bc the catalog tuples have different contents
        expect_identical(summary(g1[[1]]), summary(ds$birthyr))
    })
    ## TODO:
    ## * paths
    ## * get variable by alias, name, or URL
    ## * name<- on folder
    ## * names<- on folder
})
