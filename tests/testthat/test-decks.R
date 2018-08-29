context("Interacting with decks")

with_mock_crunch({


# Deck Catalog ------------------------------------------------------------
    ds <- loadDataset("test ds")
    test_that("newDeck posts correctly", {
        expect_POST(
            newDeck(ds, "deck1"),
            'https://app.crunch.io/api/datasets/1/decks/',
            '{"element":"shoji:entity","body":{"name":"deck1"}}'
        )
    })
    deck_cat <- decks(ds)
    test_that("Deck Catalog can be retrieved", {
        expect_is(deck_cat, "DeckCatalog")
        expect_equal(length(deck_cat), 1)
        expect_equal(names(deck_cat), "Main Deck")
    })

    main_deck <- deck_cat[[1]]


# Crunch Decks ------------------------------------------------------------

    test_that("Deck metadata", {
        expect_is(main_deck, "CrunchDeck")
        expect_equal(length(main_deck), 3)
        expect_equal(titles(main_deck), c("birthyr", "mymrset", "mymrset"))
        expect_equal(subtitles(main_deck), c("", "", "age"))
    })

    test_that("deck assignment produces POST", {
        expect_POST(
            deck_cat[["new_deck"]] <- main_deck,
            'https://app.crunch.io/api/datasets/1/decks/',
            '{"name":"new_deck","description":"","is_public":false}'
        )
    })

    test_that("deck catalog show method", {
        expected <- as.data.frame(deck_cat)[c("name", "team", "is_public", "owner_name")]
        expect_prints(
            deck_cat,
            get_output(expected)
        )
    })

    test_that("deck name and description getters and setters", {

        expect_equal(name(main_deck), "Main Deck")
        expect_equal(description(main_deck), "")
        expect_PATCH(
            name(main_deck) <- "new_name",
            'https://app.crunch.io/api/datasets/1/decks/8ad82b6b050447708aaa4eea5dd1afc1/',
            '{"name":"new_name"}'
            )
        expect_PATCH(
            description(main_deck) <- "new_description",
            'https://app.crunch.io/api/datasets/1/decks/8ad82b6b050447708aaa4eea5dd1afc1/',
            '{"description":"new_description"}'
            )
    })

    test_that("is.public method for decks", {
        expect_false(is.public(main_deck))
        expect_PATCH(
            is.public(main_deck) <- TRUE,
            'https://app.crunch.io/api/datasets/1/decks/8ad82b6b050447708aaa4eea5dd1afc1/',
            '{"is_public":true}'
            )
    })

    test_that("titles and subtitles assignment generates the correct PATCH", {
        expect_PATCH(
            titles(main_deck)[1] <- "birthyr",
            'https://app.crunch.io/api/datasets/1/decks/8ad82b6b050447708aaa4eea5dd1afc1/slides/',
            '{"https://app.crunch.io/api/datasets/1/decks/8ad82b6b050447708aaa4eea5dd1afc1/slides/da16186d29bf46e39a2fcaaa20d43ccc/":{',
            '"analysis_url":"https://app.crunch.io/api/datasets/1/decks/8ad82b6b050447708aaa4eea5dd1afc1/slides/da16186d29bf46e39a2fcaaa20d43ccc/analyses/bce96ae7d0aa4f5ea5fc1ff82ebbcb42/",',
            '"display_settings":{',
            '"percentageDirection":{"value":"colPct"},',
            '"showEmpty":{"value":false},',
            '"showMean":{"value":true},',
            '"vizType":{"value":"table"},',
            '"countsOrPercents":{"value":"percent"},',
            '"decimalPlaces":{"value":1},',
            '"populationMagnitude":{"value":3},',
            '"showSignif":{"value":true},',
            '"currentTab":{"value":0},',
            '"uiView":{"value":"app.datasets.browse"}},',
            '"subtitle":"",',
            '"title":"birthyr"}'
        )
        expect_PATCH(
            subtitles(main_deck)[1] <- "new_sub",
            'https://app.crunch.io/api/datasets/1/decks/8ad82b6b050447708aaa4eea5dd1afc1/slides/',
            '{"https://app.crunch.io/api/datasets/1/decks/8ad82b6b050447708aaa4eea5dd1afc1/slides/da16186d29bf46e39a2fcaaa20d43ccc/":{',
            '"analysis_url":"https://app.crunch.io/api/datasets/1/decks/8ad82b6b050447708aaa4eea5dd1afc1/slides/da16186d29bf46e39a2fcaaa20d43ccc/analyses/bce96ae7d0aa4f5ea5fc1ff82ebbcb42/",',
            '"display_settings":{',
            '"percentageDirection":{"value":"colPct"},',
            '"showEmpty":{"value":false},',
            '"showMean":{"value":true},',
            '"vizType":{"value":"table"},',
            '"countsOrPercents":{"value":"percent"},',
            '"decimalPlaces":{"value":1},',
            '"populationMagnitude":{"value":3},',
            '"showSignif":{"value":true},',
            '"currentTab":{"value":0},',
            '"uiView":{"value":"app.datasets.browse"}},',
            '"subtitle":"new_sub",',
            '"title":"birthyr"'
        )


        test_that("subset CrunchDecks", {
            slide <- main_deck[[1]]
            expect_is(slide, "CrunchSlide")
        })
        test_that("cube methods for crunch decks", {
            cube <- cube(main_deck[[1]])
            expect_is(cube, "CrunchCube")
            cube_list <- cubes(main_deck)
            expect_is(cube_list, "list")
            expect_identical(cube, cube_list[[1]])
        })


# Crunch Slides -----------------------------------------------------------

        test_that("Slide show method", {
            slide <- main_deck[[1]]
            expect_prints(
                slide,
                get_output(cube(slide))
            )
        })
        test_that("slide subsetting", {
            browser()
            an_cat <- analyses(slide)
            expect_is(an_cat, "AnalysisCatalog")
            expect_equal(length(an_cat), 1)
            expect_is(an_cat[[1]], "Analysis")
        })


# Analyses ----------------------------------------------------------------




    })

})

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

    settings <- list(
        percentageDirection = "colPct",
        showEmpty = FALSE,
        vizType = "histogram",
        countsOrPercents = "percent",
        decimalPlaces = 1L,
        populationMagnitude = 3L,
        showSignif = FALSE,
        currentTab = 0L,
        uiView = "app.datasets.analyze"
    )

    test_that("slides can be added to a deck", {

        univariate_slide <- newSlide(
            deck,
            query = ~v1,
            display_settings = settings,
            title = "slide1",
            subtitle = "one analysis")
        expect_is(univariate_slide, "CrunchSlide")
        anCat <- analyses(univariate_slide)
        expect_is(anCat, "AnalysisCatalog")
        expect_equal(length(anCat), 1)

        multiple_analyses <- newSlide(deck, list(~v1, ~v2), settings, title = "slide2", subtitle = "two analyses")
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
        expect_identical(cube(analysis), crtabs(~v1, ds))
        cube_list <- cubes(anCat)
        expect_is(cube_list, "list")
        expect_equal(length(cube_list), length(anCat))
        expect_identical(cube_list[[1]], crtabs(~v1, ds))
    })

    test_that("An analysis can be turned into a cube", {
        expect_identical(cube(analysis), crtabs( ~v1, ds))
    })

    test_that("cubes on an analysis catalog returns a list of cubes", {
        ancat <- analyses(slide)
        cube_list <- cubes(ancat)
        expect_is(cube_list, "list")
        expect_identical(length(cube_list), length(ancat))
        expect_identical(cube_list[[1]], crtabs(~v1, ds))
    })

    test_that("Formula's can be assigned to analyses", {
        query(analysis) <- ~v3
        expect_identical(cube(analysis), crtabs(~v3, ds))
    })

    test_that("display settings can be gotten and set", {
        ancat <- analyses(slide)
        analysis <- ancat[[1]]
        settings <- displaySettings(analysis)
        expect_is(settings, "list")
        expect_equal(
            names(settings),
            c("percentageDirection", "showEmpty", "vizType", "countsOrPercents",
              "decimalPlaces", "populationMagnitude", "showSignif", "currentTab",
              "uiView"
            )
        )
        expect_euqal(settings$countsOrPercents, "percent")
        settings$countsOrPercents <- "count"
        displaySettings(analysis) <- settings
    })
})