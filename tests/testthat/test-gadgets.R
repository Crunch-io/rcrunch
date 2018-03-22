context("shiny gadgets")

# Unit tests ----
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
    str <- "no quotes"
    expect_identical(escapeQuotes(str), str)
})

test_that("buildArrayCall", {
    expect_identical(
        buildArrayCall(ds_name = "mtcars",
            array_type = "Multiple Response",
            object_name = "ds2",
            array_var_name = "newVar",
            vars_selected = c("cyl", "mpg"),
            mr_selection = c("selected", "picked")),
        "ds2 <- makeMR(mtcars[ ,c('cyl', 'mpg')], name = 'newVar', selections = c('selected', 'picked'))"
    )
    expect_identical(
        buildArrayCall(ds_name = "mtcars",
            array_type = "Categorical Array",
            object_name = "ds2",
            array_var_name = "newVar",
            vars_selected = c("cyl", "mpg")),
        "ds2 <- makeArray(mtcars[ ,c('cyl', 'mpg')], name = 'newVar')"
    )
    expect_identical(
        buildArrayCall(ds_name = "mtcars",
            array_type = "Categorical Array",
            array_var_name = "newVar",
            vars_selected = c("cyl", "mpg")),
        "makeArray(mtcars[ ,c('cyl', 'mpg')], name = 'newVar')"
    )
})

# Mock tests ----

with_mock_crunch({
    test_that("getCrunchDataset errors", {
        expect_error(getCrunchDatasets(parent.env(environment()),
            "No CrunchDatasets detected.")
        )
    })
    ds <- loadDataset("test ds")
    test_that("getCrunchDataset", {
        l <- getCrunchDatasets(parent.env(environment()))
        expect_is(l, "list")
        expect_is(l[[1]], "CrunchDataset")
        expect_equivalent(names(l), "ds")
    })
    test_that("generateCategoryCheckboxes produces correct HTML", {
        tag <- generateCategoryCheckboxes(ds, "location", "Multiple Response")
        expected_html <- c("<div id=\"mr_selection\" class=\"form-group shiny-input-checkboxgroup shiny-input-container\">",
            "  <label class=\"control-label\" for=\"mr_selection\">Selection Categories</label>",
            "  <div class=\"shiny-options-group\">", "    <div class=\"checkbox\">",
            "      <label>", "        <input type=\"checkbox\" name=\"mr_selection\" value=\"London\"/>",
            "        <span>London</span>", "      </label>", "    </div>",
            "    <div class=\"checkbox\">", "      <label>", "        <input type=\"checkbox\" name=\"mr_selection\" value=\"Scotland\"/>",
            "        <span>Scotland</span>", "      </label>", "    </div>",
            "    <div class=\"checkbox\">", "      <label>", "        <input type=\"checkbox\" name=\"mr_selection\" value=\"No Data\"/>",
            "        <span>No Data</span>", "      </label>", "    </div>",
            "  </div>", "</div>")
        expect_is(tag, "shiny.tag")
        expect_identical(capture.output(tag), expected_html)

        tag <- generateCategoryCheckboxes(ds, c("location", "gender"), "Multiple Response")
        expected_html <- "<p style=\"color:red\">Error: selected variables have inconsistent categories.</p>"
        expect_is(tag, "shiny.tag")
        expect_identical(capture.output(tag), expected_html)

        tag <- generateCategoryCheckboxes(ds, character(0), array_type = "Multiple Response")
        expected_html <- "<p style=\"color:red\">Error: No variables selected.</p>"
        expect_is(tag, "shiny.tag")
        expect_identical(capture.output(tag), expected_html)
    })
})

# Integration tests ----
with_test_authentication({
    test_that("gadget doesn't error", {
        listDatasetGadget(autoclose = TRUE)
    })
})


