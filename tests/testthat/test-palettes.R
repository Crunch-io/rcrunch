with_mock_crunch({
    ds <- cachedLoadDataset("Vegetables example")
    palettes <- palettes(ds)

    test_that("Can get palettes from dataset", {
        expect_is(palettes, "AnalyticPalettes")
        expect_is(palettes[[1]], "AnalyticPalette")
        expect_is(palettes[["purple palette for fixture"]], "AnalyticPalette")
        expect_equal(
            palettes[[1]]$palette,
            c("#4fc3f7", "#4dd0e1", "#4db6ac", "#81c783", "#aed581", "#dce775", "#cddc39", "#fdae6b")
        )
    })

    test_that("Can print palettes", {
        expect_prints(
            fixed = FALSE,
            palettes,
            paste0(
                "                                         type default  palette\n",
                "Default green palette for fixture qualitative    TRUE 8 colors",
                ".+"
            )
        )
    })

    test_that("Can print a palette", {
        expect_prints(
            palettes[["Default green palette for fixture"]],
            paste0(
                "Crunch AnalyticPalette ", dQuote("Default green palette for fixture"), " (qualitative, default)\n",
                "#4fc3f7 #4dd0e1 #4db6ac #81c783 #aed581 #dce775 #cddc39 #fdae6b"
            )
        )
    })

    test_that("Can get default palette from dataset or palette list", {
        expect_equal(defaultPalette(ds), palettes[["Default green palette for fixture"]])
        expect_equal(defaultPalette(palettes), palettes[["Default green palette for fixture"]])
    })

})
