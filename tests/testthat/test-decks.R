context("Interacting with decks")

with_test_authentication({
    ds <- newDataset(df)
    test_that("decks can be created", {
        expect_is(decks(ds), "DeckCatalog")
        expect_equal(length(decks(ds)), 0)
        main_deck <- newDeck(ds, "MainDeck")
        expect_is(main_deck, "CrunchDeck")
        deck_cat <- decks(ds)
        expect_identical(deck_cat[[1]], main_deck)
        expect_equal(name(main_deck), "MainDeck")
    })
    deck <- newDeck(ds, "deck")
    test_that("deck name getters and setters", {
        expect_equal(name(deck), "deck")
        name(deck) <- "new_name"
        expect_equal(name(deck), "new_name")
    })
    test_that("deck description", {
        expect_equal(description(deck), "")
        description(deck) <- "description"
        expect_equal(description(deck), "description")
    })
    test_that("setting deck publicness", {
        expect_false(is.public(deck))
        is.public(deck) <- TRUE
        expect_true(is.public(deck))
    })

    test_that("slides can be added to a deck", {
        univariate_slide <- newSlide(deck, ~v1, title = "slide1", subtitle = "one analysis")
        expect_is(univariate_slide, "CrunchSlide")
        anCat <- analyses(univariate_slide)
        expect_is(anCat, "AnalysisCatalog")
        expect_equal(length(anCat), 1)

        multiple_analyses <- newSlide(deck, list(~v1, ~v2),  title = "slide2", subtitle = "two analyses")
        expect_is(multiple_analyses, "CrunchSlide")
        expect_equal(length(analyses(multiple_analyses)), 2)
    })

    test_that("deck titles and subtitles", {
        expect_equal(titles(deck), c("slide1", "slide2"))
        expect_equal(subtitles(deck), c("one analysis", "two analyses"))
        titles(deck) <- c("new_title1", "new_title2")
        expect_equal(titles(deck)[1], "new_title1")
        subtitles(deck) <- c("new-one-analysis", "new-two-analyses")
        expect_equal(subtitles(deck)[1], "new-one-analysis")
        titles(deck)[1] <- "slide1"
        expect_equal(titles(deck)[1], "slide1")
        subtitles(deck)[1] <- "one analysis"
        expect_equal(subtitles(deck)[1],  "one analysis")
    })

    test_that("Decks can be created by assignment", {
        deckCat <- decks(ds)
        deckCat[["Deck 2"]] <- deck
        expect_equal(length(decks(ds)), 3)
        expect_true("Deck 2" %in% names(deckCat))
    })
    slideCat <- slides(deck)
    test_that("slides method",{
        expect_is(slideCat, "SlideCatalog")
        expect_equal(length(slideCat), 2)
        expect_equal(titles(slideCat), c("slide1", "new_title2"))
    })
    test_that("slides can be added by assignment", {
        slide <- slideCat[[2]]
        expect_is(slide, "CrunchSlide")
        slideCat[[3]] <- slide
        expect_is(slideCat[[3]], "CrunchSlide")
        expect_equal(length(slideCat), 3)
    })
    test_that("slide title and subtitle", {
        slide <- slideCat[[2]]
        expect_equal(title(slide), "new_title2")
    })

    slide <- slideCat[[2]]
    anCat <- analyses(slide)

    test_that("analyses", {
        expect_is(anCat, "AnalysisCatalog")
        expect_equal(length(anCat), 2)
    })

    analysis <- anCat[[1]]

    test_that("Analysis Catalog subset", {
        expect_is(analysis, "Analysis")
    })
    test_that("analyses can be cubed", {
        expect_identical(cube(analysis), crtabs(~v2, ds))
        cube_list <- cubes(anCat)
        expect_is(cube_list, "list")
        expect_equal(length(cube_list), length(anCat))
        expect_identical(cube_list[[1]], crtabs(~v2, ds))
    })

    test_that("An analysis can be turned into a cube", {
        expect_identical(cube(analysis), crtabs( ~v2, ds))
    })

    test_that("cubes on an analysis catalog returns a list of cubes", {
        ancat <- analyses(slide)
        cube_list <- cubes(ancat)
        expect_is(cube_list, "list")
        expect_identical(length(cube_list), length(ancat))
        expect_identical(cube_list[[1]], crtabs(~v3, ds))
    })

    test_that("Formula's can be assigned to analyses", {
        query(analysis) <- ~v3
        expect_identical(cube(analysis), crtabs(~v3, ds))
    })
})