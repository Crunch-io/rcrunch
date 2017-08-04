context("Cube dimensions")

with_mock_crunch({
    ## Load a ton of cube fixtures via the tab book feature
    ds <- loadDataset("test ds")
    m <- multitables(ds)[[1]]
    with_POST("https://app.crunch.io/api/datasets/1/multitables/apidocs-tabbook/", {
        book <- tabBook(m, data=ds)
    })
    cube <- book[[2]][[2]]
    d <- dimensions(cube)
    test_that("Dimensions of a cube", {
        expect_is(d, "CubeDims")
    })
    test_that("Dimnames", {
        expect_identical(dimnames(d),
            list(
                q1=c("Cat", "Dog", "Bird", "Skipped", "Not Asked"),
                allpets=c("Cat", "Dog", "Bird", "<NA>", "<NA>", "<NA>")
            ))
        expect_identical(names(dimnames(d)), c("q1", "allpets"))
    })
    test_that("Additional variable attributes on CubeDims", {
        expect_identical(names(variables(d)), c("Pet", "All pets owned"))
        expect_identical(aliases(variables(d)), c("q1", "allpets"))
        expect_identical(descriptions(variables(d)),
            c("What is your favorite pet?",
            "Do you have any of these animals as pets? Please select all that apply."))
    })
    test_that("Getting those additional variable attributes from the cube", {
        expect_identical(names(cube), c("Pet", "All pets owned"))
        expect_identical(aliases(cube), c("q1", "allpets"))
        expect_identical(descriptions(cube),
            c("What is your favorite pet?",
            "Do you have any of these animals as pets? Please select all that apply."))
    })
    test_that("Getting those attributes from MultitableResult (the column variables)", {
        expect_identical(names(book[[1]]), c("Total", "All pets owned", "Pet"))
        expect_identical(aliases(book[[1]]), c("total", "allpets", "q1"))
        expect_identical(descriptions(book[[1]]),
            c(NA,
            "Do you have any of these animals as pets? Please select all that apply.",
            "What is your favorite pet?"))
    })
    test_that("Getting those attributes from TabBookResult (the row/sheet variables)", {
        expect_identical(names(book)[2:4],
            c("Pet", "Pets by location", "Number of dogs"))
        expect_identical(aliases(book)[2:4], c("q1", "petloc", "ndogs"))
        expect_identical(descriptions(book)[2:4],
            c("What is your favorite pet?",
            "Name the kinds of pets you have at these locations.",
            NA))
    })
})
