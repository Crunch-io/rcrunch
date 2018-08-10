context("shiny gadgets")

test_that("buildLoadDatasetCall", {
    expect_identical(
        buildLoadDatasetCall("Personal Project", "data", "ds"),
        "ds <- loadDataset('data')"
    )
    expect_identical(
        buildLoadDatasetCall("proj", "data", "ds"),
        "ds <- loadDataset('data', project = 'proj')"
    )
    expect_identical(
        buildLoadDatasetCall("Personal Project", "data"),
        "loadDataset('data')"
    )
    expect_identical(
        buildLoadDatasetCall("proj", "data"),
        "loadDataset('data', project = 'proj')"
    )
    expect_identical(
        buildLoadDatasetCall("Personal Project", "weird's dataset", "ds"),
        "ds <- loadDataset('weird\\'s dataset')"
    )
})

test_that("escapeQuotes", {
    expect_identical(escapeQuotes("test's tests"), "test\\'s tests")
    str <- "no quotes"
    expect_identical(escapeQuotes(str), str)
})

with_mock_crunch({
    test_that("listDatasets gets expected input from other Crunch functions", {
        expect_is(names(projects()), "character")
        expect_is(listDatasets(), "character")
    })
})
