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

    expect_identical(deck_cat[["Main Deck"]], deck_cat[[1]])
    expect_error(
        deck_cat[["Not a name"]],
        paste0(dQuote("Not a name"), " is not present in deck catalog")
    )
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

    test_that("deck titles and subtitles", {
        expect_equal(titles(main_deck), c("birthyr", "mymrset", "mymrset"))
        expect_PATCH(
            titles(main_deck) <- paste("slide", 1:3),
            'https://app.crunch.io/api/datasets/1/decks/8ad82b6b050447708aaa4eea5dd1afc1/slides/',
            '{"https://app.crunch.io/api/datasets/1/decks/8ad82b6b050447708aaa4eea5dd1afc1/slides/da16186d29bf46e39a2fcaaa20d43ccc/":{"title":"slide 1"},',
            '"https://app.crunch.io/api/datasets/1/decks/8ad82b6b050447708aaa4eea5dd1afc1/slides/5938aef59f6f42aeaafd7651781030e4/":{"title":"slide 2"},',
            '"https://app.crunch.io/api/datasets/1/decks/8ad82b6b050447708aaa4eea5dd1afc1/slides/72ea814337434af596f2cab41441553f/":{"title":"slide 3"}}'
        )
        expect_PATCH(
            subtitles(main_deck) <- paste("slide", 1:3),
            'https://app.crunch.io/api/datasets/1/decks/8ad82b6b050447708aaa4eea5dd1afc1/slides/',
            '{"https://app.crunch.io/api/datasets/1/decks/8ad82b6b050447708aaa4eea5dd1afc1/slides/da16186d29bf46e39a2fcaaa20d43ccc/":{"subtitle":"slide 1"},',
            '"https://app.crunch.io/api/datasets/1/decks/8ad82b6b050447708aaa4eea5dd1afc1/slides/5938aef59f6f42aeaafd7651781030e4/":{"subtitle":"slide 2"},',
            '"https://app.crunch.io/api/datasets/1/decks/8ad82b6b050447708aaa4eea5dd1afc1/slides/72ea814337434af596f2cab41441553f/":{"subtitle":"slide 3"}}'
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

    test_that("export decks generates correct POST", {
        expect_POST(
            exportDeck(main_deck, type = "json"),
            'https://app.crunch.io/api/datasets/1/decks/8ad82b6b050447708aaa4eea5dd1afc1/export/',
            '')
        expect_POST(
            exportDeck(main_deck, type = "xlsx"),
            'https://app.crunch.io/api/datasets/1/decks/8ad82b6b050447708aaa4eea5dd1afc1/export/',
            '')
    })

    test_that("deck assignment", {
        expect_POST(
            main_deck[[4]] <- main_deck[[1]],
            'https://app.crunch.io/api/datasets/1/decks/8ad82b6b050447708aaa4eea5dd1afc1/slides/',
            '{"element":"shoji:entity",',
            '"body":{"title":"birthyr",',
            '"subtitle":"",',
            '"analyses":[{"query":{"measures":{"count":{"function":"cube_count","args":[]}},',
            '"dimensions":[{"function":"bin","args":[{"variable":"https://app.crunch.io/api/datasets/1/variables/000002/"}]}],',
            '"weight":null},',
            '"query_environment":{"filter":[],"weight":null},',
            '"display_settings":{"decimalPlaces":{"value":1},"percentageDirection":{"value":"colPct"},"vizType":{"value":"table"},"countsOrPercents":{"value":"percent"},"uiView":{"value":"app.datasets.browse"}}}]}}'
        )
    })
    test_that("delete decks", {
        with_consent({
            expect_DELETE(
                delete(main_deck),
                'https://app.crunch.io/api/datasets/1/decks/8ad82b6b050447708aaa4eea5dd1afc1/'
            )
        })
    })

    # Slide Catalog -----------------------------------------------------------

    test_that("moveLastElement", {
        expect_equal(moveLastElement(1:5, 3), c(1,2,5,4))
        expect_equal(moveLastElement(1:5, 5), c(1,2,3,4))
        expect_equal(moveLastElement(1:5, 1), c(5,2,3,4))
        expect_equal(moveLastElement(1:5, 2), c(1,5,3,4))
    })

    test_that("reorderSlides", {
        expect_PATCH(
            reorderSlides(slides(main_deck), c(3, 2, 1)),
            'https://app.crunch.io/api/datasets/1/decks/8ad82b6b050447708aaa4eea5dd1afc1/slides/flat',
            '{"element":"shoji:order",',
            '"self":"https://app.crunch.io/api/datasets/1/decks/8ad82b6b050447708aaa4eea5dd1afc1/slides/flat/",',
            '"description":"Order of the slides on this deck",',
            '"graph":["https://app.crunch.io/api/datasets/1/decks/8ad82b6b050447708aaa4eea5dd1afc1/slides/72ea814337434af596f2cab41441553f/",',
            '"https://app.crunch.io/api/datasets/1/decks/8ad82b6b050447708aaa4eea5dd1afc1/slides/5938aef59f6f42aeaafd7651781030e4/",',
            '"https://app.crunch.io/api/datasets/1/decks/8ad82b6b050447708aaa4eea5dd1afc1/slides/da16186d29bf46e39a2fcaaa20d43ccc/"]}'
        )
    })


    # Crunch Slides -----------------------------------------------------------

    test_that("New Slide", {
        expect_POST(
            newSlide(main_deck, ~birthyr, title = "Title", subtitle = "SubTitle"),
            'https://app.crunch.io/api/datasets/1/decks/8ad82b6b050447708aaa4eea5dd1afc1/slides/',
            '{"element":"shoji:entity",',
            '"body":{"title":"Title",',
            '"subtitle":"SubTitle",',
            '"analyses":[{"query":{"dimensions":[{"variable":"https://app.crunch.io/api/datasets/1/variables/birthyr/"}],',
            '"measures":{"count":{"function":"cube_count","args":[]}}},',
            '"display_settings":{"percentageDirection":{"value":"colPct"},',
            '"showEmpty":{"value":false},',
            '"showMean":{"value":false},',
            '"vizType":{"value":"table"},',
            '"countsOrPercents":{"value":"percent"},',
            '"decimalPlaces":{"value":1},',
            '"populationMagnitude":{"value":3},',
            '"showSignif":{"value":true},',
            '"currentTab":{"value":0},',
            '"uiView":{"value":"app.datasets.browse"}}}]}}'
        )
    })

    test_that("spliceDisplaySettings works correctly", {
        display_names <- c("percentageDirection", "showEmpty", "showMean", "vizType",
                           "countsOrPercents", "decimalPlaces", "populationMagnitude", "showSignif",
                           "currentTab", "uiView")
        settings <- spliceDisplaySettings(list(decimalPlaces = 2))
        expect_equal(length(settings), 10)
        expect_equal(names(settings), display_names)
        expect_equal(settings$decimalPlaces, 2)

        settings <- spliceDisplaySettings(list(decimalPlaces = 2, showMean = TRUE))
        expect_equal(length(settings), 10)
        expect_equal(names(settings), display_names)
        expect_equal(settings$decimalPlaces, 2)
        expect_equal(settings$showMean, TRUE)

        expect_warning(
            settings <<- spliceDisplaySettings(list(notAsetting = TRUE)),
            paste0("Invalid display settings ommitted: ", dQuote('notAsetting'))
        )
        expect_equal(length(settings), 10)
        expect_equal(names(settings), display_names)
    })
    slide <- main_deck[[1]]
    test_that("Slide show method", {
        expect_prints(
            slide,
            get_output(cube(slide))
        )
    })

    test_that("Slide titles and subtitles", {
        expect_equal(title(slide), "birthyr")
        expect_equal(subtitle(slide), "")
        expect_PATCH(
            title(slide) <- "new_title",
            'https://app.crunch.io/api/datasets/1/decks/8ad82b6b050447708aaa4eea5dd1afc1/slides/da16186d29bf46e39a2fcaaa20d43ccc/',
            '{"element":"shoji:entity","body":{"title":"new_title"}}'
        )
        expect_PATCH(
            subtitle(slide) <- "new_subtitle",
            'https://app.crunch.io/api/datasets/1/decks/8ad82b6b050447708aaa4eea5dd1afc1/slides/da16186d29bf46e39a2fcaaa20d43ccc/',
            '{"element":"shoji:entity","body":{"subtitle":"new_subtitle"}}'
        )
    })

    slide <- main_deck[[1]]
    an_cat <- analyses(slide)

    test_that("slide subsetting", {
        expect_is(an_cat, "AnalysisCatalog")
        expect_equal(length(an_cat), 1)
        expect_is(an_cat[[1]], "Analysis")
        expect_identical(analysis(slide), an_cat[[1]])
    })
    test_that("query assignment for slides", {
        expect_PATCH(
            query(slide) <- ~birthyr,
            'https://app.crunch.io/api/datasets/1/decks/8ad82b6b050447708aaa4eea5dd1afc1/slides/da16186d29bf46e39a2fcaaa20d43ccc/analyses/bce96ae7d0aa4f5ea5fc1ff82ebbcb42/',
            '{"element":"shoji:entity",',
            '"body":{"query":{"dimensions":[',
            '{"variable":"https://app.crunch.io/api/datasets/1/variables/birthyr/"}],',
            '"measures":{"count":{"function":"cube_count","args":[]}}}}}'
        )
    })

    test_that("Subset Crunch Slide", {
        an <- slide[[1]]
        expect_is(an, "Analysis")
    })

    test_that("delete slide", {
        with_consent({
            expect_DELETE(
                delete(slide),
                'https://app.crunch.io/api/datasets/1/decks/8ad82b6b050447708aaa4eea5dd1afc1/'
            )
        })
    })


    # Analyses ----------------------------------------------------------------

    test_that("Analysis Assignment", {
        payload <- paste0(
            '{"element":"shoji:entity",',
            '"body":{"query":{',
            '"measures":{"count":{"function":"cube_count","args":[]}},',
            '"dimensions":[{"function":"bin","args":[{"variable":"https://app.crunch.io/api/datasets/1/variables/000002/"}]}],',
            '"weight":null},',
            '"display_settings":{"percentageDirection":{"value":"colPct"},',
            '"showEmpty":{"value":false},',
            '"showMean":{"value":true},',
            '"vizType":{"value":"table"},',
            '"countsOrPercents":{"value":"percent"},',
            '"decimalPlaces":{"value":1},',
            '"populationMagnitude":{"value":3},',
            '"showSignif":{"value":true},',
            '"currentTab":{"value":0},',
            '"uiView":{"value":"app.datasets.browse"}},',
            '"query_environment":{"filter":[],"weight":null}}}'
        )
        expect_POST(
            slide[[2]] <- slide[[1]],
            'https://app.crunch.io/api/datasets/1/decks/8ad82b6b050447708aaa4eea5dd1afc1/slides/da16186d29bf46e39a2fcaaa20d43ccc/analyses/',
            payload)
        expect_PATCH(
            slide[[1]] <- slide[[1]],
            'https://app.crunch.io/api/datasets/1/decks/8ad82b6b050447708aaa4eea5dd1afc1/slides/da16186d29bf46e39a2fcaaa20d43ccc/analyses/bce96ae7d0aa4f5ea5fc1ff82ebbcb42/',
            payload)

    })

    test_that("analysis assignment errors", {
        expect_error(
            an_cat[[1]] <- list(~birthyr, ~gender),
            "Invalid assignment. You tried to assign 2 formulas to 1 analysis."
        )
    })




    test_that("analysis display settings", {
        analysis <- an_cat[[1]]
        settings <- displaySettings(analysis)
        expect_is(settings, "list")
        expect_equal(length(settings), 10)
        expect_equal(settings$decimalPlaces, 1)
        expect_equal(
            names(settings),
            c("percentageDirection", "showEmpty", "showMean", "vizType",
              "countsOrPercents", "decimalPlaces", "populationMagnitude", "showSignif",
              "currentTab", "uiView")
        )
        expect_PATCH(
            displaySettings(analysis) <- list(decimalPlaces = 1),
            'https://app.crunch.io/api/datasets/1/decks/8ad82b6b050447708aaa4eea5dd1afc1/slides/da16186d29bf46e39a2fcaaa20d43ccc/analyses/bce96ae7d0aa4f5ea5fc1ff82ebbcb42/',
            '{"element":"shoji:entity",',
            '"body":{"display_settings":{',
            '"percentageDirection":{"value":"colPct"},',
            '"showEmpty":{"value":false},',
            '"showMean":{"value":true},',
            '"vizType":{"value":"table"},',
            '"countsOrPercents":{"value":"percent"},',
            '"decimalPlaces":{"value":1},',
            '"populationMagnitude":{"value":3},',
            '"showSignif":{"value":true},',
            '"currentTab":{"value":0},',
            '"uiView":{"value":"app.datasets.browse"}}}}'
        )
    })

    test_that("AnalysisCatelog display settings", {
        ancat <- analyses(slide)
        settings_list <- displaySettings(ancat)
        expect_is(settings_list, "list")
        expect_identical(displaySettings(ancat), displaySettings(ancat[[1]]))
        expect_PATCH(
            displaySettings(ancat) <- list(decimalPlaces = 2),
            'https://app.crunch.io/api/datasets/1/decks/8ad82b6b050447708aaa4eea5dd1afc1/slides/da16186d29bf46e39a2fcaaa20d43ccc/analyses/bce96ae7d0aa4f5ea5fc1ff82ebbcb42/',
            '{"element":"shoji:entity",',
            '"body":{"display_settings":{',
            '"percentageDirection":{"value":"colPct"},',
            '"showEmpty":{"value":false},',
            '"showMean":{"value":true},',
            '"vizType":{"value":"table"},',
            '"countsOrPercents":{"value":"percent"},',
            '"decimalPlaces":{"value":2},',
            '"populationMagnitude":{"value":3},',
            '"showSignif":{"value":true},',
            '"currentTab":{"value":0},',
            '"uiView":{"value":"app.datasets.browse"}}}}'
        )
    })

    test_that("Assigning formulas to analysis", {
        ancat <- analyses(slide)
        expect_error(
            ancat[[5]] <- ~birthyr,
            "Index out of bounds, you can only assign a formula to an existing analysis."
        )
        expect_PATCH(
            ancat[[1]] <- ~birthyr + gender,
            'https://app.crunch.io/api/datasets/1/decks/8ad82b6b050447708aaa4eea5dd1afc1/slides/da16186d29bf46e39a2fcaaa20d43ccc/analyses/bce96ae7d0aa4f5ea5fc1ff82ebbcb42/',
            '{"element":"shoji:entity",',
            '"body":{"query":{"dimensions":[',
            '{"variable":"https://app.crunch.io/api/datasets/1/variables/birthyr/"},',
            '{"variable":"https://app.crunch.io/api/datasets/1/variables/gender/"}],',
            '"measures":{"count":{"function":"cube_count","args":[]}}}}}'
        )
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
            c("percentageDirection", "showEmpty", "showMean", "vizType", "countsOrPercents",
              "decimalPlaces", "populationMagnitude", "showSignif", "currentTab",
              "uiView"
            )
        )
        expect_equal(settings$countsOrPercents, "percent")
        settings$countsOrPercents <- "count"
        displaySettings(analysis) <- settings
    })
})