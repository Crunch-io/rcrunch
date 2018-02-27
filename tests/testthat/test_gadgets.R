context("shiny gadgets")

test_that("buildLoadDatasetCall", {
    expect_identical(buildLoadDatasetCall("Personal Project", "data", "ds"),
        "ds <- loadDataset('data')")
    expect_identical(buildLoadDatasetCall("proj", "data", "ds"),
        "ds <- loadDataset('data', project = 'proj')")
    expect_identical(buildLoadDatasetCall("Personal Project", "data"),
        "loadDataset('data')")
    expect_identical(buildLoadDatasetCall("proj", "data"),
        "loadDataset('data', project = 'proj')")
    expect_identical(buildLoadDatasetCall("Personal Project", "weird's dataset", "ds"),
        "ds <- loadDataset('weird\\'s dataset')")
})

test_that("escapeQuotes", {
    expect_identical(escapeQuotes("test's tests"), "test\\'s tests")
    expect_identical(escapeQuotes('test "the test" tests'), "test \\\"the test\\\" tests")
    expect_identical(escapeQuotes("Mr. roger's \"neighborhood\""), "Mr. roger\\'s \\\"neighborhood\\\"")
    str <- "no quotes"
    expect_identical(escapeQuotes(str), str)
}
)