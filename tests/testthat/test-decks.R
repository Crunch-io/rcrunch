context("Interacting with decks")

with_mock_crunch({
    # Deck Catalog ------------------------------------------------------------
    ds <- loadDataset("test ds")
    test_that("newDeck posts correctly", {
        expect_POST(
            newDeck(ds, "deck1"),
            "https://app.crunch.io/api/datasets/1/decks/",
            '{"element":"shoji:entity","body":{"name":"deck1"}}'
        )
    })
    deck_cat <- decks(ds)
    test_that("Deck Catalog can be retrieved", {
        expect_is(deck_cat, "DeckCatalog")
        expect_equal(length(deck_cat), 2)
        expect_equal(names(deck_cat), c("Main Deck", "Main Deck"))
    })
    expect_warning(
        expect_GET(
            deck_cat[["Main Deck"]],
            "https://app.crunch.io/api/datasets/1/decks/f9b502afe2e54e79b3e17f3cc61174ae/"
        ),
        paste0(
            dQuote("Main Deck"),
            " does not uniquely identify elements. Returning the first match"
        )
    )
    expect_error(
        deck_cat[["Not a name"]],
        paste0(dQuote("Not a name"), " is not present in deck catalog")
    )
    expect_error(
        deck_cat[[c("name 1", "name 2")]],
        "You can only select one deck at a time"
    )
    main_deck <- deck_cat[[2]]

    # Crunch Decks ------------------------------------------------------------

    test_that("Deck metadata", {
        expect_is(main_deck, "CrunchDeck")
        expect_equal(length(main_deck), 3)
        expect_equal(titles(main_deck), c("birthyr", "mymrset", "mymrset"))
        expect_equal(subtitles(main_deck), c("", "", "age"))
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
            "https://app.crunch.io/api/datasets/1/decks/8ad8/",
            '{"name":"new_name"}'
        )

        expect_PATCH(
            description(main_deck) <- "new_description",
            "https://app.crunch.io/api/datasets/1/decks/8ad8/",
            '{"description":"new_description"}'
        )
    })

    test_that("deck titles and subtitles", {
        expect_equal(titles(main_deck), c("birthyr", "mymrset", "mymrset"))
        expect_PATCH(
            titles(main_deck) <- paste("slide", 1:3),
            "https://app.crunch.io/api/datasets/1/decks/8ad8/slides/",
            '{"element":"shoji:catalog","index":{',
            '"https://app.crunch.io/api/datasets/1/decks/8ad8/slides/da161/":',
            '{"title":"slide 1"},',
            '"https://app.crunch.io/api/datasets/1/decks/8ad8/slides/5938/":',
            '{"title":"slide 2"},',
            '"https://app.crunch.io/api/datasets/1/decks/8ad8/slides/72e8/":',
            '{"title":"slide 3"}}}'
        )
        expect_PATCH(
            subtitles(main_deck) <- paste("slide", 1:3),
            "https://app.crunch.io/api/datasets/1/decks/8ad8/slides/",
            '{"element":"shoji:catalog","index":{',
            '"https://app.crunch.io/api/datasets/1/decks/8ad8/slides/da161/":',
            '{"subtitle":"slide 1"},',
            '"https://app.crunch.io/api/datasets/1/decks/8ad8/slides/5938/":',
            '{"subtitle":"slide 2"},',
            '"https://app.crunch.io/api/datasets/1/decks/8ad8/slides/72e8/":',
            '{"subtitle":"slide 3"}}}'
        )
    })

    test_that("is.public method for decks", {
        expect_false(is.public(main_deck))
        expect_PATCH(
            is.public(main_deck) <- TRUE,
            "https://app.crunch.io/api/datasets/1/decks/8ad8/",
            '{"is_public":true}'
        )
        expect_no_request(is.public(deck_cat[[2]]) <- FALSE)
    })

    test_that("teams on decks", {
        expect_null(team(deck_cat[[2]]))
        expect_PATCH(
            team(deck_cat[[2]]) <- getTeams()[[1]],
            "https://app.crunch.io/api/datasets/1/decks/8ad8/",
            '{"team":"https://app.crunch.io/api/teams/team1/"}'
        )
        expect_no_request(team(deck_cat[[2]]) <- NULL)
        expect_error(
            team(deck_cat[[2]]) <- 4.2,
            "Team setting requires either a CrunchTeam entity, URL, or NULL"
        )
    })

    test_that("subset CrunchDecks", {
        expect_is(main_deck[[1]], "CrunchSlide")
    })
    test_that("cube methods for crunch decks", {
        cube <- cube(main_deck[[1]])
        expect_is(cube, "CrunchCube")
        cube_list <- cubes(main_deck)
        expect_is(cube_list, "list")
        expect_identical(cube, cube_list[[1]])
    })
    test_that("export decks generates correct POST", {
        expect_header(
            expect_POST(
                exportDeck(main_deck, format = "json"),
                "https://app.crunch.io/api/datasets/1/decks/8ad8/export/"
            ),
            "Accept: application/json"
        )
        expect_header(
            expect_POST(
                exportDeck(main_deck, format = "xlsx"),
                "https://app.crunch.io/api/datasets/1/decks/8ad8/export/"
            ),
            "Accept: application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
        )
        expect_header(
            expect_POST(
                exportDeck(main_deck, format = "pptx"),
                "https://app.crunch.io/api/datasets/1/decks/8ad8/export/"
            ),
            "Accept: application/vnd.openxmlformats-officedocument.presentationml.presentation"
        )
    })

    test_that("Deck export errors helpfully", {
        expect_error(
            exportDeck(ds, format = "json"),
            "exportDeck is only available for CrunchDecks."
        )
    })

    test_that("deck assignment", {
        expect_POST(
            main_deck[[4]] <- main_deck[[1]],
            "https://app.crunch.io/api/datasets/1/decks/8ad8/slides/",
            '{"element":"shoji:entity",',
            '"body":{"title":"birthyr",',
            '"subtitle":"",',
            '"analyses":[{"query":{"measures":{"count":{"function":"cube_count","args":[]}},',
            '"dimensions":[{"function":"bin","args":[{"variable":"https://app.',
            'crunch.io/api/datasets/1/variables/000002/"}]}],',
            '"weight":null},',
            '"query_environment":{"filter":[],"weight":null},',
            '"display_settings":',
            '{"percentageDirection":{"value":"colPct"},',
            '"showEmpty":{"value":false},',
            '"showMean":{"value":true},',
            '"vizType":{"value":"table"},',
            '"countsOrPercents":{"value":"percent"},',
            '"decimalPlaces":{"value":1},',
            '"populationMagnitude":{"value":3},',
            '"showSignif":{"value":true},',
            '"currentTab":{"value":0},',
            '"uiView":{"value":"app.datasets.browse"}}}]}}'
        )
    })
    test_that("delete decks", {
        with_consent({
            expect_DELETE(
                delete(main_deck),
                "https://app.crunch.io/api/datasets/1/decks/8ad8/"
            )
        })
    })

    # Slide Catalog -----------------------------------------------------------

    test_that("moveLastElement", {
        expect_equal(moveLastElement(1:5, 3), c(1, 2, 5, 4))
        expect_equal(moveLastElement(1:5, 5), c(1, 2, 3, 4))
        expect_equal(moveLastElement(1:5, 1), c(5, 2, 3, 4))
        expect_equal(moveLastElement(1:5, 2), c(1, 5, 3, 4))
    })

    test_that("reorderSlides", {
        expect_PATCH(
            reorderSlides(slides(main_deck), c(3, 2, 1)),
            "https://app.crunch.io/api/datasets/1/decks/8ad8/slides/flat",
            '{"element":"shoji:order",',
            '"self":"https://app.crunch.io/api/datasets/1/decks/8ad8/slides/flat/",',
            '"description":"Order of the slides on this deck",',
            '"graph":["https://app.crunch.io/api/datasets/1/decks/8ad8/slides/72e8/",',
            '"https://app.crunch.io/api/datasets/1/decks/8ad8/slides/5938/",',
            '"https://app.crunch.io/api/datasets/1/decks/8ad8/slides/da161/"]}'
        )
    })


    # Crunch Slides -----------------------------------------------------------

    test_that("New Slide", {
        expect_POST(
            newSlide(main_deck, ~birthyr, title = "Title", subtitle = "SubTitle"),
            "https://app.crunch.io/api/datasets/1/decks/8ad8/slides/",
            '{"element":"shoji:entity",',
            '"body":{"title":"Title",',
            '"subtitle":"SubTitle",',
            '"analyses":[{"query":{"dimensions":[{"variable":"https://app.',
            'crunch.io/api/datasets/1/variables/birthyr/"}],',
            '"measures":{"count":{"function":"cube_count","args":[]}}},',
            '"display_settings":{"percentageDirection":{"value":"colPct"},',
            '"showEmpty":{"value":false},',
            '"showMean":{"value":false},',
            '"vizType":{"value":"table"},',
            '"countsOrPercents":{"value":"percent"},',
            '"decimalPlaces":{"value":1},',
            '"showSignif":{"value":true},',
            '"currentTab":{"value":0}}}]}}'
        )
    })

    slide <- main_deck[[1]]
    test_that("Slide show method", {
        expect_prints(
            slide,
            get_output(cube(slide))
        )
    })

    test_that("Slide titles and subtitles", {
        expect_PATCH(
            names(main_deck) <- c("new_name", "other_new_name", "other_new_name"),
            "https://app.crunch.io/api/datasets/1/decks/8ad8/slides/",
            '{"element":"shoji:catalog","index":',
            '{"https://app.crunch.io/api/datasets/1/decks/8ad8/slides/da161/":{"title":"new_name"},',
            '"https://app.crunch.io/api/datasets/1/decks/8ad8/slides/5938/":{"title":"other_new_name"},',
            '"https://app.crunch.io/api/datasets/1/decks/8ad8/slides/72e8/":{"title":"other_new_name"}}}'
        )

        expect_PATCH(
            titles(main_deck) <- c("new_name", "other_new_name", "other_new_name"),
            "https://app.crunch.io/api/datasets/1/decks/8ad8/slides/",
            '{"element":"shoji:catalog","index":',
            '{"https://app.crunch.io/api/datasets/1/decks/8ad8/slides/da161/":{"title":"new_name"},',
            '"https://app.crunch.io/api/datasets/1/decks/8ad8/slides/5938/":{"title":"other_new_name"},',
            '"https://app.crunch.io/api/datasets/1/decks/8ad8/slides/72e8/":{"title":"other_new_name"}}}'
        )
    })

    test_that("Slide titles and subtitles", {
        expect_equal(title(slide), "birthyr")
        expect_equal(subtitle(slide), "")
        expect_PATCH(
            title(slide) <- "new_title",
            "https://app.crunch.io/api/datasets/1/decks/8ad8/slides/da161/",
            '{"title":"new_title"}'
        )
        expect_PATCH(
            subtitle(slide) <- "new_subtitle",
            "https://app.crunch.io/api/datasets/1/decks/8ad8/slides/da161/",
            '{"subtitle":"new_subtitle"}'
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
            "https://app.crunch.io/api/datasets/1/decks/8ad8/slides/da161/analyses/bce96/",
            '{"element":"shoji:entity",',
            '"body":{"query":{"dimensions":[',
            '{"variable":"https://app.crunch.io/api/datasets/1/variables/birthyr/"}],',
            '"measures":{"count":{"function":"cube_count","args":[]}}}}}'
        )
    })

    test_that("query assignment for slides, convenience function with analysis", {
        expect_PATCH(
            analysis(slide) <- ~birthyr,
            "https://app.crunch.io/api/datasets/1/decks/8ad8/slides/da161/analyses/bce96/",
            '{"element":"shoji:entity",',
            '"body":{"query":{"dimensions":[',
            '{"variable":"https://app.crunch.io/api/datasets/1/variables/birthyr/"}],',
            '"measures":{"count":{"function":"cube_count","args":[]}}}}}'
        )
    })

    test_that("query assignment for slides via subset methods", {
        expect_PATCH(
            analysis(decks(ds)[[2]][[1]]) <- ~birthyr,
            "https://app.crunch.io/api/datasets/1/decks/8ad8/slides/da161/analyses/bce96/",
            '{"element":"shoji:entity",',
            '"body":{"query":{"dimensions":[',
            '{"variable":"https://app.crunch.io/api/datasets/1/variables/birthyr/"}],',
            '"measures":{"count":{"function":"cube_count","args":[]}}}}}'
        )

        expect_PATCH(
            analysis(main_deck[[1]]) <- ~birthyr,
            "https://app.crunch.io/api/datasets/1/decks/8ad8/slides/da161/analyses/bce96/",
            '{"element":"shoji:entity",',
            '"body":{"query":{"dimensions":[',
            '{"variable":"https://app.crunch.io/api/datasets/1/variables/birthyr/"}],',
            '"measures":{"count":{"function":"cube_count","args":[]}}}}}'
        )

        expect_PATCH(
            query(main_deck[[1]]) <- ~birthyr,
            "https://app.crunch.io/api/datasets/1/decks/8ad8/slides/da161/analyses/bce96/",
            '{"element":"shoji:entity",',
            '"body":{"query":{"dimensions":[',
            '{"variable":"https://app.crunch.io/api/datasets/1/variables/birthyr/"}],',
            '"measures":{"count":{"function":"cube_count","args":[]}}}}}'
        )

        expect_PATCH(
            query(analysis(main_deck[[1]])) <- ~birthyr,
            "https://app.crunch.io/api/datasets/1/decks/8ad8/slides/da161/analyses/bce96/",
            '{"element":"shoji:entity",',
            '"body":{"query":{"dimensions":[',
            '{"variable":"https://app.crunch.io/api/datasets/1/variables/birthyr/"}],',
            '"measures":{"count":{"function":"cube_count","args":[]}}}}}'
        )
    })

    test_that("filter display for slides (and analyses)", {
        expect_identical(filter(slide), NULL)
        expect_prints(filter(slide), "NULL")
        # filter() on slide and analysis are identical (a shortcut when there is
        # one analysis)
        expect_identical(filter(slide), filter(analysis(slide)))

        # main_deck[[2]] has a saved filter (though it has two analyses)
        expect_is(filter(analyses(main_deck[[2]])[[1]]), "CrunchFilter")
        expect_prints(
            filter(analyses(main_deck[[2]])[[1]]),
            'Crunch filter .*Occasional Political Interest.*\nExpression: gender %in% "Male"',
            fixed = FALSE
        )

        # main_deck[[3]] has an adhoc filter
        expect_is(filter(main_deck[[3]]), "CrunchExpr")
        expect_prints(
            filter(main_deck[[3]]),
            'Crunch expression: gender %in% "Male"'
        )
        # filter() on slide and analysis are identical (a shortcut when there is
        # one analysis)
        expect_identical(filter(main_deck[[3]]), filter(analysis(main_deck[[3]])))
    })

    test_that("filter<-something for slides (and analyses)", {
        # though CrunchLogicalExpr could be made into filters, the expression in
        # query_environment is a bespoke shape unlike any other expression.
        # This can be enabled once the specialness here is removed on the server.
        expect_error(
            filter(decks(ds)[[2]][[3]]) <- ds$birthyr > 1990,
            "Setting adhoc filters on decks is unsupported"
        )
        expect_error(
            filter(analysis(decks(ds)[[2]][[3]])) <- ds$birthyr > 1990,
            "Setting adhoc filters on decks is unsupported"
        )

        # named filters (through a CrunchDeck object)
        expect_PATCH(
            filter(main_deck[[3]]) <- filters(ds)[["Occasional Political Interest"]],
            "https://app.crunch.io/api/datasets/1/decks/8ad8/slides/72e8/analyses/52fb/",
            '{"query_environment":{"filter":["https://app.crunch.io/api/datasets/1/filters/filter1/"]}'
        )
        expect_PATCH(
            filter(analysis(main_deck[[3]])) <- filters(ds)[["Public filter"]],
            "https://app.crunch.io/api/datasets/1/decks/8ad8/slides/72e8/analyses/52fb/",
            '{"query_environment":{"filter":["https://app.crunch.io/api/datasets/1/filters/filter2/"]}'
        )

        # named filters (through teh decks catalog)
        expect_PATCH(
            filter(decks(ds)[[2]][[3]]) <- filters(ds)[["Occasional Political Interest"]],
            "https://app.crunch.io/api/datasets/1/decks/8ad8/slides/72e8/analyses/52fb/",
            '{"query_environment":{"filter":["https://app.crunch.io/api/datasets/1/filters/filter1/"]}'
        )
        expect_PATCH(
            filter(analysis(decks(ds)[[2]][[3]])) <- filters(ds)[["Public filter"]],
            "https://app.crunch.io/api/datasets/1/decks/8ad8/slides/72e8/analyses/52fb/",
            '{"query_environment":{"filter":["https://app.crunch.io/api/datasets/1/filters/filter2/"]}'
        )
    })

    test_that("filter<-NULL for slides (and analyses)", {
        expect_PATCH(
            filter(decks(ds)[[2]][[3]]) <- NULL,
            "https://app.crunch.io/api/datasets/1/decks/8ad8/slides/72e8/analyses/52fb/",
            '{"query_environment":{"filter":[]}}'
        )
        expect_PATCH(
            filter(analysis(decks(ds)[[2]][[3]])) <- NULL,
            "https://app.crunch.io/api/datasets/1/decks/8ad8/slides/72e8/analyses/52fb/",
            '{"query_environment":{"filter":[]}}'
        )
        expect_PATCH(
            filter(main_deck[[3]]) <- NULL,
            "https://app.crunch.io/api/datasets/1/decks/8ad8/slides/72e8/analyses/52fb/",
            '{"query_environment":{"filter":[]}}'
        )
        expect_PATCH(
            filter(analysis(main_deck[[3]])) <- NULL,
            "https://app.crunch.io/api/datasets/1/decks/8ad8/slides/72e8/analyses/52fb/",
            '{"query_environment":{"filter":[]}}'
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
                "https://app.crunch.io/api/datasets/1/decks/8ad8/"
            )
        })
    })


    # Analyses ----------------------------------------------------------------

    test_that("Analysis Catalog is ordered correctly", {
        anCat <- analyses(main_deck[[2]])
        expect_equal(length(anCat), 2)

        # This checks that the analyses are ordered according to
        # the ".../slides/5938/analyses/order.json object and not
        # the index of the json file. Currently the only slides which
        # will have more than one analysis are profiles slides, so this
        # is somewhat of an edge case for the R package.

        expect_equal(
            names(index(anCat)),
            c(
                "https://app.crunch.io/api/datasets/1/decks/8ad8/slides/5938/analyses/3f2e3/",
                "https://app.crunch.io/api/datasets/1/decks/8ad8/slides/5938/analyses/3f2e2/"
            )
        )
    })

    test_that("Analysis Assignment", {
        payload <- paste0(
            '{"element":"shoji:entity",',
            '"body":{"query":{',
            '"measures":{"count":{"function":"cube_count","args":[]}},',
            '"dimensions":[{"function":"bin","args":[{"variable":"https://app.',
            'crunch.io/api/datasets/1/variables/000002/"}]}],',
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
            "https://app.crunch.io/api/datasets/1/decks/8ad8/slides/da161/analyses/",
            payload
        )
        expect_PATCH(
            slide[[1]] <- slide[[1]],
            "https://app.crunch.io/api/datasets/1/decks/8ad8/slides/da161/analyses/bce96/",
            payload
        )
    })

    test_that("analysis assignment errors", {
        expect_error(
            an_cat[[1]] <- list(~birthyr, ~gender),
            "Invalid assignment. You tried to assign 2 formulas to 1 analysis."
        )
        expect_error(
            an_cat[[1]] <- list(~birthyr, "gender"),
            "Entry 2 is not a formula"
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
            c(
                "percentageDirection", "showEmpty", "showMean", "vizType",
                "countsOrPercents", "decimalPlaces", "populationMagnitude", "showSignif",
                "currentTab", "uiView"
            )
        )
        expect_PATCH(
            displaySettings(analysis) <- list(decimalPlaces = 1),
            "https://app.crunch.io/api/datasets/1/decks/8ad8/slides/da161/analyses/bce96/",
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

        # and the same thing works with the convenience of specifying the slide
        expect_identical(settings, displaySettings(slide))
        expect_PATCH(
            displaySettings(slide) <- list(decimalPlaces = 1),
            "https://app.crunch.io/api/datasets/1/decks/8ad8/slides/da161/analyses/bce96/",
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

    test_that("AnalysisCatalog display settings", {
        ancat <- analyses(slide)
        settings_list <- displaySettings(ancat)
        expect_is(settings_list, "list")
        expect_identical(displaySettings(ancat), displaySettings(ancat[[1]]))
        expect_PATCH(
            displaySettings(ancat) <- list(decimalPlaces = 2),
            "https://app.crunch.io/api/datasets/1/decks/8ad8/slides/da161/analyses/bce96/",
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
            ancat[[1]] <- ~ birthyr + gender,
            "https://app.crunch.io/api/datasets/1/decks/8ad8/slides/da161/analyses/bce96/",
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
            subtitle = "one analysis"
        )
        expect_is(univariate_slide, "CrunchSlide")
        anCat <- analyses(univariate_slide)
        expect_is(anCat, "AnalysisCatalog")
        expect_equal(length(anCat), 1)
    })

    test_that("deck titles and subtitles", {
        slide_2 <- newSlide(deck, ~v2, settings, title = "slide2", subtitle = "two analyses")
        expect_equal(titles(deck), c("slide1", "slide2"))
        expect_equal(subtitles(deck), c("one analysis", "two analyses"))
        titles(deck) <- c("new_title1", "new_title2")
        expect_equal(titles(deck)[1], "new_title1")
        subtitles(deck) <- c("new-one-analysis", "new-two-analyses")
        expect_equal(subtitles(deck)[1], "new-one-analysis")
        titles(deck)[1] <- "slide1"
        expect_equal(titles(deck)[1], "slide1")
        subtitles(deck)[1] <- "one analysis"
        expect_equal(subtitles(deck)[1], "one analysis")
    })

    slideCat <- slides(deck)
    test_that("slides method", {
        expect_is(slideCat, "SlideCatalog")
        expect_equal(length(slideCat), 2)
        expect_equal(titles(slideCat), c("slide1", "new_title2"))
        expect_equal(names(slideCat), c("slide1", "new_title2"))
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
        expect_equal(length(anCat), 1)
    })

    analysis <- anCat[[1]]

    test_that("Analysis Catalog subset", {
        expect_is(analysis, "Analysis")
    })
    test_that("analyses can be cubed", {
        expect_identical(cube(analysis), crtabs(~v2, ds))
    })

    test_that("cubes on an analysis catalog returns a list of cubes", {
        ancat <- analyses(slide)
        cube_list <- cubes(ancat)
        expect_is(cube_list, "list")
        expect_identical(length(cube_list), length(ancat))
        expect_identical(cube_list[[1]], crtabs(~v2, ds))
    })

    test_that("Formulas can be assigned to analyses", {
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
            c(
                "percentageDirection", "showEmpty", "showMean", "vizType", "countsOrPercents",
                "decimalPlaces", "populationMagnitude", "showSignif", "currentTab",
                "uiView"
            )
        )
        expect_equal(settings$countsOrPercents, "percent")
        settings$countsOrPercents <- "count"
        displaySettings(analysis) <- settings
    })

    ds <- refresh(ds)
    deck <- decks(ds)[["new_name"]]

    test_that("query setting", {
        # establish that we have three slides, and their queries
        expect_equal(length(slides(deck)), 3)
        expect_identical(cube(deck[[1]]), crtabs(~v1, ds))
        expect_identical(cube(deck[[2]]), crtabs(~v3, ds))
        expect_identical(cube(deck[[3]]), crtabs(~v2, ds))

        # change queries
        query(deck[[1]]) <- ~v3
        query(analysis(deck[[2]])) <- ~v4

        # make sure that the slides are all the same (except the one we replaced)
        deck <- refresh(deck)
        expect_equal(length(slides(deck)), 3)
        expect_identical(cube(deck[[1]]), crtabs(~v3, ds))
        expect_identical(cube(deck[[2]]), crtabs(~v4, ds))
        expect_identical(cube(deck[[3]]), crtabs(~v2, ds))
    })

    test_that("Filter setting", {
        ds <- refresh(ds)
        deck <- decks(ds)[["new_name"]]

        # establish that we have three slides, and their queries
        expect_equal(length(slides(deck)), 3)
        expect_identical(cube(deck[[1]]), crtabs(~v3, ds))
        expect_identical(cube(deck[[2]]), crtabs(~v4, ds))
        expect_identical(cube(deck[[3]]), crtabs(~v2, ds))

        # make a named filter
        filters(ds)[["v4 is B"]] <- ds$v4 == "B"
        filters(ds)[["v1 over 0"]] <- ds$v1 > 0

        # add filters
        filter(deck[[1]]) <- filters(ds)[["v4 is B"]]
        filter(analysis(deck[[2]])) <- filters(ds)[["v1 over 0"]]

        # check filters
        expect_identical(filter(deck[[1]]), filters(ds)[["v4 is B"]])
        expect_identical(filter(analysis(deck[[2]])), filters(ds)[["v1 over 0"]])

        # remove filters
        filter(deck[[1]]) <- NULL
        filter(analysis(deck[[2]])) <- NULL

        # make sure that the slides are still all the same
        deck <- refresh(deck)
        expect_equal(length(slides(deck)), 3)
        expect_identical(cube(deck[[1]]), crtabs(~v3, ds))
        expect_identical(cube(deck[[2]]), crtabs(~v4, ds))
        expect_identical(cube(deck[[3]]), crtabs(~v2, ds))
    })
})
