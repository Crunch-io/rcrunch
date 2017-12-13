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
        expect_identical(types(variables(d)),
            c("categorical", "subvariable_items"))
    })
    test_that("Getting those additional variable attributes from the cube", {
        expect_identical(names(cube), c("Pet", "All pets owned"))
        expect_identical(aliases(cube), c("q1", "allpets"))
        expect_identical(descriptions(cube),
            c("What is your favorite pet?",
            "Do you have any of these animals as pets? Please select all that apply."))
        expect_identical(types(cube),
            c("categorical", "subvariable_items"))
        expect_identical(notes(cube),
            c(NA_character_, NA_character_))
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

    test_that("'measures' metadata", {
        expect_identical(names(measures(crtabs(max(birthyr) ~ 1, data=ds))),
            "Birth Year")
        expect_identical(names(variables(crtabs(max(birthyr) ~ 1, data=ds))),
            "Birth Year")
        expect_length(measures(crtabs(~ gender + textVar, data=ds)), 0)
        expect_identical(names(variables(crtabs(~ gender + textVar, data=ds))),
            c("Gender", "Text variable ftw"))
        skip("'mean' doesn't return variable metadata like 'max' does")
        expect_identical(names(measures(crtabs(mean(birthyr) ~ gender + textVar, data=ds))),
            "Birth Year")
        expect_identical(names(variables(crtabs(mean(birthyr) ~ gender + textVar, data=ds))),
            c("Gender", "Text variable ftw", "Birth Year"))
        expect_identical(names(measures(crtabs(list(mean(birthyr), max(birthyr)) ~ gender + textVar, data=ds))),
            c("Birth Year", "Birth Year"))
        expect_identical(names(variables(crtabs(list(mean(birthyr), max(birthyr)) ~ gender + textVar, data=ds))),
            c("Gender", "Text variable ftw", "Birth Year")) ## De-duped
        expect_identical(names(measures(crtabs(list(mean(birthyr), max(starttime)) ~ gender + textVar, data=ds))),
            c("Interview Start Time", "Birth Year")) ## Order in the JSON is reversed
        expect_identical(names(variables(crtabs(list(mean(birthyr), max(starttime)) ~ gender + textVar, data=ds))),
            c("Gender", "Text variable ftw", "Interview Start Time", "Birth Year"))
    })
})
