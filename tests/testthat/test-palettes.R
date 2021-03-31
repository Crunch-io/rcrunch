with_mock_crunch({
    ds <- cachedLoadDataset("Vegetables example")
    palettes <- palettes(ds)

    test_that("Can get palettes from dataset", {
        expect_is(palettes, "AnalyticPalettes")
        expect_is(palettes[[1]], "AnalyticPalette")
        expect_is(palettes[["purple palette"]], "AnalyticPalette")
        expect_equal(
            palettes[[1]]$palette,
            c("#340043", "#640083", "#9100bf", "#c300ff", "#e17fff")
        )
    })

    test_that("Can print palettes", {
        expect_prints(
            palettes,
            paste0(
                "                                   type default  palette\n",
                "purple palette              qualitative   FALSE 5 colors\n",
                "Green Palette for dashboard qualitative    TRUE 8 colors"
            )
        )
    })

    test_that("Can print a palette", {
        expect_prints(
            palettes[[1]],
            paste0(
                "Crunch AnalyticPalette ", dQuote("purple palette"), " (qualitative)\n",
                "#340043 #640083 #9100bf #c300ff #e17fff"
            )
        )
    })

    test_that("Can get default palette from dataset or palette list", {
        expect_equal(defaultPalette(ds), palettes[[2]])
        expect_equal(defaultPalette(palettes), palettes[[2]])
    })

})
