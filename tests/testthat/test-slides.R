with_mock_crunch({
    ds <- cachedLoadDataset("test ds deck")
    main_deck <- decks(ds)[[2]]

    # Old fixtures haven't kept up with the API so use vegetables dataset
    # to test printing
    ds_veg <- cachedLoadDataset("Vegetables example")
    deck_veg <- decks(ds_veg)[[2]]

    # Crunch Slides -----------------------------------------------------------

    test_that("New Slide", {
        expect_POST(
            newSlide(
                main_deck, ~birthyr, title = "Title", subtitle = "SubTitle",
                weight = ds$birthyr, filter = filters(ds)[["Occasional Political Interest"]]),
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/",
            '{"element":"shoji:entity",',
            '"body":{"title":"Title",',
            '"subtitle":"SubTitle",',
            '"analyses":[{"query":{"dimensions":[{"variable":"https://app.',
            'crunch.io/api/datasets/4/variables/birthyr/"}],',
            '"measures":{"count":{"function":"cube_count","args":[]}},',
            '"weight":"https://app.crunch.io/api/datasets/4/variables/birthyr/"},',
            '"display_settings":{"percentageDirection":{"value":"colPct"},',
            '"showEmpty":{"value":false},',
            '"showMean":{"value":false},',
            '"vizType":{"value":"table"},',
            '"countsOrPercents":{"value":"percent"},',
            '"decimalPlaces":{"value":1},',
            '"showSignif":{"value":true},',
            '"currentTab":{"value":0}},"query_environment":{',
            '"filter":[{"filter":"https://app.crunch.io/api/datasets/4/filters/filter1/"}],',
            '"weight":"https://app.crunch.io/api/datasets/4/variables/birthyr/"}}]}}'
        )
    })

    test_that("New Slide - specify analyses", {
        example_analyses <- list(list(
            query = list(
                dimensions = list(list(variable = self(ds$birthyr))),
                measures = list(count = list(`function` = "cube_count", args = list()))
            )
        ))
        example_analyses_json <- paste0(
            '"analyses":[{"query":{"dimensions":[{"variable":"https://app.',
            'crunch.io/api/datasets/4/variables/birthyr/"}],',
            '"measures":{"count":{"function":"cube_count","args":[]}'
        )

        expect_POST(
            newSlide(main_deck, NULL, title = "title", analyses = example_analyses),
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/",
            '{"element":"shoji:entity",',
            '"body":{"title":"title","subtitle":"",', example_analyses_json, "}}"
        )

        expect_error(
            newSlide(main_deck, ~birthyr, analyses = example_analyses),
            "Cannot specify both a `query` and `analyses` for `newSlide()`",
            fixed = TRUE
        )

        expect_error(
            newSlide(main_deck, NULL),
            "Must specify either a `query` or `analyses` for `newSlide()`",
            fixed = TRUE
        )
    })

    test_that("New Slide - with manual transforms", {
        expect_POST(
            newSlide(
                main_deck, ~gender, title = "Title", subtitle = "SubTitle",
                transform = list(
                    rows_dimension = list(elements = list(`1` = list("hide" = TRUE))),
                    version = "1.0"
                )
            ),
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/",
            '{"element":"shoji:entity",',
            '"body":{"title":"Title",',
            '"subtitle":"SubTitle",',
            '"analyses":[{"query":{"dimensions":[{"variable":"https://app.',
            'crunch.io/api/datasets/4/variables/gender/"}],',
            '"measures":{"count":{"function":"cube_count","args":[]}}},',
            '"display_settings":{"percentageDirection":{"value":"colPct"},',
            '"showEmpty":{"value":false},',
            '"showMean":{"value":false},',
            '"vizType":{"value":"table"},',
            '"countsOrPercents":{"value":"percent"},',
            '"decimalPlaces":{"value":1},',
            '"showSignif":{"value":true},',
            '"currentTab":{"value":0}},"transform":{',
            '"rows_dimension":{"elements":{"1":{"hide":true}}},"version":"1.0"}}]}}'
        )
    })

    test_that("New slide - prevents ca categories in first dim", {
        expect_error(
            newSlide(
                main_deck,
                ~categories(catarray) + subvariables(catarray) + mymrset,
                title = "Title"
            ),
            "First dimension of .+ analysis cannot be .+ categories"
        )
    })

    test_that("New markdown slide", {
        expect_POST(
            newMarkdownSlide(
                main_deck, title = "Title", subtitle = "SubTitle", "#Heading\n\n", "*text*"
            ),
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/",
            '{"element":"shoji:entity",',
            '"body":{"type":"markdown","markdown":"#Heading\\n\\n*text*",',
            '"title":"Title","subtitle":"SubTitle"}}'
        )
    })

    test_that("New markdown slide (named ... arg)", {
        expect_POST(
            newMarkdownSlide(
                main_deck, title = "Title", subtitle = "SubTitle", "abc", other_arg = 1
            ),
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/",
            '{"element":"shoji:entity",',
            '"body":{"type":"markdown","markdown":"abc",',
            '"title":"Title","subtitle":"SubTitle","other_arg":1}}'
        )
    })

    test_that("markdownSlideImage", {
        temp_file <- tempfile(fileext = ".png")

        expect_error(
            markdownSlideImage(temp_file),
            "Could not find file"
        )

        write(raw(1), temp_file)
        expect_true(grepl(
            "^!\\[.+\\.png\\]\\(data:image/png;base64,.+\\)$",
            markdownSlideImage(temp_file)
        ))
    })

    slide <- main_deck[[1]]

    test_that("Slide titles and subtitles", {
        expect_PATCH(
            names(main_deck) <- c("new_name", "other_new_name", "other_new_name", "another"),
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/",
            '{"element":"shoji:catalog","index":',
            '{"https://app.crunch.io/api/datasets/4/decks/8ad8/slides/da161/":',
            '{"title":"new_name"},',
            '"https://app.crunch.io/api/datasets/4/decks/8ad8/slides/5938/":',
            '{"title":"other_new_name"},',
            '"https://app.crunch.io/api/datasets/4/decks/8ad8/slides/72e8/":',
            '{"title":"other_new_name"},',
            '"https://app.crunch.io/api/datasets/4/decks/8ad8/slides/72e9/":',
            '{"title":"another"}}}'
        )

        expect_PATCH(
            titles(main_deck) <- c("new_name", "other_new_name", "other_new_name", "another"),
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/",
            '{"element":"shoji:catalog","index":',
            '{"https://app.crunch.io/api/datasets/4/decks/8ad8/slides/da161/":',
            '{"title":"new_name"},',
            '"https://app.crunch.io/api/datasets/4/decks/8ad8/slides/5938/":',
            '{"title":"other_new_name"},',
            '"https://app.crunch.io/api/datasets/4/decks/8ad8/slides/72e8/":',
            '{"title":"other_new_name"},',
            '"https://app.crunch.io/api/datasets/4/decks/8ad8/slides/72e9/":',
            '{"title":"another"}}}'
        )
    })

    test_that("Slide titles and subtitles", {
        expect_equal(title(slide), "birthyr")
        expect_equal(subtitle(slide), "")
        expect_PATCH(
            title(slide) <- "new_title",
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/da161/",
            '{"title":"new_title"}'
        )
        expect_PATCH(
            subtitle(slide) <- "new_subtitle",
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/da161/",
            '{"subtitle":"new_subtitle"}'
        )
    })

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
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/da161/analyses/bce96/",
            '{"element":"shoji:entity",',
            '"body":{"query":{"dimensions":[',
            '{"variable":"https://app.crunch.io/api/datasets/4/variables/birthyr/"}],',
            '"measures":{"count":{"function":"cube_count","args":[]}}}}}'
        )
    })

    test_that("query assignment for slides, convenience function with analysis", {
        expect_PATCH(
            analysis(slide) <- ~birthyr,
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/da161/analyses/bce96/",
            '{"element":"shoji:entity",',
            '"body":{"query":{"dimensions":[',
            '{"variable":"https://app.crunch.io/api/datasets/4/variables/birthyr/"}],',
            '"measures":{"count":{"function":"cube_count","args":[]}}}}}'
        )
    })

    test_that("query assignment for slides via subset methods", {
        expect_PATCH(
            analysis(decks(ds)[[2]][[1]]) <- ~birthyr,
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/da161/analyses/bce96/",
            '{"element":"shoji:entity",',
            '"body":{"query":{"dimensions":[',
            '{"variable":"https://app.crunch.io/api/datasets/4/variables/birthyr/"}],',
            '"measures":{"count":{"function":"cube_count","args":[]}}}}}'
        )

        expect_PATCH(
            analysis(main_deck[[1]]) <- ~birthyr,
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/da161/analyses/bce96/",
            '{"element":"shoji:entity",',
            '"body":{"query":{"dimensions":[',
            '{"variable":"https://app.crunch.io/api/datasets/4/variables/birthyr/"}],',
            '"measures":{"count":{"function":"cube_count","args":[]}}}}}'
        )

        expect_PATCH(
            query(main_deck[[1]]) <- ~birthyr,
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/da161/analyses/bce96/",
            '{"element":"shoji:entity",',
            '"body":{"query":{"dimensions":[',
            '{"variable":"https://app.crunch.io/api/datasets/4/variables/birthyr/"}],',
            '"measures":{"count":{"function":"cube_count","args":[]}}}}}'
        )

        expect_PATCH(
            query(analysis(main_deck[[1]])) <- ~birthyr,
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/da161/analyses/bce96/",
            '{"element":"shoji:entity",',
            '"body":{"query":{"dimensions":[',
            '{"variable":"https://app.crunch.io/api/datasets/4/variables/birthyr/"}],',
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
            'Crunch logical expression: gender %in% "Male"'
        )
        # filter() on slide and analysis are identical (a shortcut when there is
        # one analysis)
        expect_identical(filter(main_deck[[3]]), filter(analysis(main_deck[[3]])))
    })

    test_that("filter<-something for slides (and analyses)", {
        # Ad-hoc expressions
        expect_PATCH(
            filter(decks(ds)[[2]][[3]]) <- ds$birthyr > 1990,
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/72e8/analyses/52fb/",
            '{"query_environment":{"filter":[{"function":">","args":[{"variable":',
            '"https://app.crunch.io/api/datasets/4/variables/birthyr/"},{"value":1990}]}],',
            '"weight":null}}'
        )
        expect_PATCH(
            filter(analysis(decks(ds)[[2]][[3]])) <- ds$birthyr > 1990,
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/72e8/analyses/52fb/",
            '{"query_environment":{"filter":[{"function":">","args":[{"variable":',
            '"https://app.crunch.io/api/datasets/4/variables/birthyr/"},{"value":1990}]}],',
            '"weight":null}}'
        )

        # named filters (through a CrunchDeck object)
        expect_PATCH(
            filter(main_deck) <- filters(ds)[["Occasional Political Interest"]],
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/da161/analyses/bce96/",
            '{"query_environment":{"filter":[{"filter":"https://app.crunch.io/api/',
            'datasets/4/filters/filter1/"}],"weight":null}'
        )

        # named filters (through a CrunchSlide)
        expect_PATCH(
            filter(main_deck[[3]]) <- filters(ds)[["Occasional Political Interest"]],
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/72e8/analyses/52fb/",
            '{"query_environment":{"filter":[{"filter":"https://app.crunch.io/api/',
            'datasets/4/filters/filter1/"}],"weight":null}'
        )
        expect_PATCH(
            filter(analysis(main_deck[[3]])) <- filters(ds)[["Public filter"]],
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/72e8/analyses/52fb/",
            '{"query_environment":{"filter":[{"filter":"https://app.crunch.io/api/',
            'datasets/4/filters/filter2/"}],"weight":null}'
        )

        # named filters (through the decks catalog)
        expect_PATCH(
            filter(decks(ds)[[2]][[3]]) <- filters(ds)[["Occasional Political Interest"]],
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/72e8/analyses/52fb/",
            '{"query_environment":{"filter":[{"filter":"https://app.crunch.io/api/',
            'datasets/4/filters/filter1/"}],"weight":null}'
        )
        expect_PATCH(
            filter(analysis(decks(ds)[[2]][[3]])) <- filters(ds)[["Public filter"]],
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/72e8/analyses/52fb/",
            '{"query_environment":{"filter":[{"filter":"https://app.crunch.io/api/',
            'datasets/4/filters/filter2/"}],"weight":null}'
        )
    })

    test_that("filter<-NULL for slides (and analyses)", {
        expect_PATCH(
            filter(decks(ds)[[2]][[3]]) <- NULL,
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/72e8/analyses/52fb/",
            '{"query_environment":{"filter":[],"weight":null}}'
        )
        expect_PATCH(
            filter(analysis(decks(ds)[[2]][[3]])) <- NULL,
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/72e8/analyses/52fb/",
            '{"query_environment":{"filter":[],"weight":null}}'
        )
        expect_PATCH(
            filter(main_deck[[3]]) <- NULL,
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/72e8/analyses/52fb/",
            '{"query_environment":{"filter":[],"weight":null}}'
        )
        expect_PATCH(
            filter(analysis(main_deck[[3]])) <- NULL,
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/72e8/analyses/52fb/",
            '{"query_environment":{"filter":[],"weight":null}}'
        )
    })

    test_that("weight display for slides (and analyses)", {
        expect_identical(weight(slide), NULL)
        expect_prints(weight(slide), "NULL")
        # filter() on slide and analysis are identical (a shortcut when there is
        # one analysis)
        expect_identical(weight(slide), weight(analysis(slide)))


        # main_deck[[4]] has a weight
        expect_is(weight(main_deck[[4]]), "CrunchVariable")
        # filter() on slide and analysis are identical (a shortcut when there is
        # one analysis)
        expect_identical(weight(main_deck[[4]]), weight(analysis(main_deck[[4]])))
    })

    test_that("weight<-something for slides (and analyses)", {
        # through a CrunchDeck object
        expect_PATCH(
            weight(main_deck) <- ds$birthyr,
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/da161/analyses/bce96/",
            '{"query_environment":{"filter":[],"weight":',
            '"https://app.crunch.io/api/datasets/4/variables/birthyr/"}'
        )
        # through a CrunchSlide object
        expect_PATCH(
            weight(main_deck[[1]]) <- ds$birthyr,
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/da161/analyses/bce96/",
            '{"query_environment":{"filter":[],"weight":',
            '"https://app.crunch.io/api/datasets/4/variables/birthyr/"}'
        )

        expect_PATCH(
            weight(analysis(main_deck[[1]])) <- ds$birthyr,
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/da161/analyses/bce96/",
            '{"query_environment":{"filter":[],"weight":',
            '"https://app.crunch.io/api/datasets/4/variables/birthyr/"}'
        )

        # through the decks catalog
        expect_PATCH(
            weight(decks(ds)[[2]][[1]]) <- ds$birthyr,
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/da161/analyses/bce96/",
            '{"query_environment":{"filter":[],"weight":',
            '"https://app.crunch.io/api/datasets/4/variables/birthyr/"}'
        )

        expect_PATCH(
            weight(decks(ds)[[2]][[1]]) <- ds$birthyr,
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/da161/analyses/bce96/",
            '{"query_environment":{"filter":[],"weight":',
            '"https://app.crunch.io/api/datasets/4/variables/birthyr/"}'
        )

        expect_error(
            weight(decks(ds)[[2]][[1]]) <- ds$mymrset,
            "is not a weightVariable"
        )
    })

    test_that("weight<-NULL for slides (and analyses)", {
        request_string <- paste0(
            '{"query_environment":{"filter":[],"weight":null},"query":{"measures":{"count":',
            '{"function":"cube_count","args":[]}},"dimensions":[{"each":',
            '"https://app.crunch.io/api/datasets/4/variables/4c51593ab88e4c5e97a99c87e53784d0/"},', #nolint
            '{"function":"as_selected","args":[{"variable":',
            '"https://app.crunch.io/api/datasets/4/variables/4c51593ab88e4c5e97a99c87e53784d0/"}]},', #nolint
            '{"function":"bin","args":[{"variable":',
            '"https://app.crunch.io/api/datasets/4/variables/0127c71ba3094ea4a12ca5823050991c/"}]}]}}' #nolint
        )
        expect_PATCH(
            weight(decks(ds)[[2]][[4]]) <- NULL,
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/72e9/analyses/52fc/",
            request_string
        )
        expect_PATCH(
            weight(analysis(decks(ds)[[2]][[4]])) <- NULL,
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/72e9/analyses/52fc/",
            request_string
        )
        expect_PATCH(
            weight(main_deck[[4]]) <- NULL,
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/72e9/analyses/52fc/",
            request_string
        )
        expect_PATCH(
            weight(analysis(main_deck[[4]])) <- NULL,
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/72e9/analyses/52fc/",
            request_string
        )
    })

    test_that("weight<- and filter<- play nicely together", {
        # Can add a weight when filter exists
        expect_PATCH(
            weight(decks(ds)[[2]][[3]]) <- ds$birthyr,
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/72e8/analyses/52fb/",
            '{"query_environment":{"filter":[{"function":"in","args":[{"variable":"gender",',
            '"dataset":"1"},{"column":[1],"type":{"function":"typeof","args":[{"variable":"',
            'gender","dataset":"1"}]}}],"name":"Adhoc filter"}],"weight":',
            '"https://app.crunch.io/api/datasets/4/variables/birthyr/"},"query":{"measures":{',
            '"count":{"function":"cube_count","args":[]}},"dimensions":[{"each":',
            '"https://app.crunch.io/api/datasets/4/variables/4c51593ab88e4c5e97a99c87e53784d0/"},',
            '{"function":"as_selected","args":[{"variable":',
            '"https://app.crunch.io/api/datasets/4/variables/4c51593ab88e4c5e97a99c87e53784d0/"',
            '}]},{"function":"bin","args":[{"variable":',
            '"https://app.crunch.io/api/datasets/4/variables/0127c71ba3094ea4a12ca5823050991c/"}]}',
            '],"weight":"https://app.crunch.io/api/datasets/4/variables/birthyr/"}}'
        )
        # Can add a filter when weight exists
        expect_PATCH(
            filter(decks(ds)[[2]][[4]]) <- filters(ds)[["Public filter"]],
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/72e9/analyses/52fc/",
            '{"query_environment":{"filter":[{"filter":"https://app.crunch.io/api/',
            'datasets/4/filters/filter2/"}],"weight":',
            '"https://app.crunch.io/api/datasets/4/variables/birthyr/"}}'
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
                "https://app.crunch.io/api/datasets/4/decks/8ad8/"
            )
        })
    })

    test_that("slide printing", {
        expect_prints(
            deck_veg[[1]],
            paste0(
                "Crunch analysis slide ", dQuote("donut"), " (donut)\n",
                "- Dimensions:\n",
                "    - healthy_eater\n",
                "- Measures: ", dQuote("count"), " (cube_count())"
            ),
            fixed = TRUE,
            crayon.enabled = FALSE
        )

        expect_prints(
            deck_veg[[2]],
            paste0(
                "Crunch analysis slide ", dQuote("table with filter and weight"),
                " | and a subtitle (table)\n",
                "- Dimensions:\n",
                "    - dimension(veg_enjoy_ca, \"subvariables\")\n",
                "    - veg_enjoy_ca\n",
                "- Measures: ", dQuote("count"), " (cube_count())\n",
                "- Filter:\n",
                "    - Crunch logical expression: age > 18\n",
                "- Weight: weight"
            ),
            fixed = TRUE,
            crayon.enabled = FALSE
        )

        expect_prints(
            deck_veg[[3]],
            paste0(
                "Crunch markdown slide <Untitled> | markdown slide (markdown)\n",
                "*markdown goes here*"
            ),
            fixed = TRUE,
            crayon.enabled = FALSE
        )
    })


    # Markdown ----------------------------------------------------------------
    test_that("can get and set slideMarkdown", {
        expect_equal(
            slideMarkdown(deck_veg[[3]]),
            "*markdown goes here*"
        )


        expect_PATCH(
            slideMarkdown(deck_veg[[3]]) <- "new markdown",
            "https://app.crunch.io/api/datasets/veg/decks/dk02/slides/dk02s03/",
            '{"markdown":"new markdown"}'
        )
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
                "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/5938/analyses/3f2e3/",
                "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/5938/analyses/3f2e2/"
            )
        )
    })

    test_that("Analysis Assignment", {
        payload <- paste0(
            '{"element":"shoji:entity",',
            '"body":{"query":{',
            '"measures":{"count":{"function":"cube_count","args":[]}},',
            '"dimensions":[{"function":"bin","args":[{"variable":"https://app.',
            'crunch.io/api/datasets/4/variables/000002/"}]}],',
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
            '"query_environment":{"filter":[],"weight":null},',
            '"viz_specs":{"default":{"format":{"show_empty":true}}}}}'
        )
        expect_POST(
            slide[[2]] <- slide[[1]],
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/da161/analyses/",
            payload
        )
        expect_PATCH(
            slide[[1]] <- slide[[1]],
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/da161/analyses/bce96/",
            payload
        )
    })

    test_that("Analysis list assignment", {
        expect_PATCH(
            analysis(slide) <- list(query = "query", display_settings = "settings"),
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/da161/analyses/bce96/",
            '{"element":"shoji:entity","body":{"query":"query",',
            '"display_settings":"settings"}}'
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
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/da161/analyses/bce96/",
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
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/da161/analyses/bce96/",
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
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/da161/analyses/bce96/",
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

    test_that("analysis viz_specs", {
        analysis <- an_cat[[1]]
        expect_equal(
            vizSpecs(analysis),
            list(default = list(format = list(show_empty = TRUE)))
        )
        expect_PATCH(
            vizSpecs(analysis) <- list(default = list(format = list(show_empty = FALSE))),
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/da161/analyses/bce96/",
            '{"element":"shoji:entity","body":{"viz_specs"',
            ':{"default":{"format":{"show_empty":false}}}}}'
        )

        # and the same thing works with the convenience of specifying the slide
        expect_identical(
            vizSpecs(slide),
            list(default = list(format = list(show_empty = TRUE)))
        )
        expect_PATCH(
            vizSpecs(slide) <- list(default = list(format = list(show_empty = FALSE))),
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/da161/analyses/bce96/",
            '{"element":"shoji:entity","body":{"viz_specs":',
            '{"default":{"format":{"show_empty":false}}}}}'
        )

        # And the analysis catalog
        expect_equal(
            vizSpecs(an_cat),
            list(default = list(format = list(show_empty = TRUE)))
        )
        expect_PATCH(
            vizSpecs(an_cat) <- list(default = list(format = list(show_empty = FALSE))),
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/da161/analyses/bce96/",
            '{"element":"shoji:entity","body":{"viz_specs":',
            '{"default":{"format":{"show_empty":false}}}}}'
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
            "https://app.crunch.io/api/datasets/4/decks/8ad8/slides/da161/analyses/bce96/",
            '{"element":"shoji:entity",',
            '"body":{"query":{"dimensions":[',
            '{"variable":"https://app.crunch.io/api/datasets/4/variables/birthyr/"},',
            '{"variable":"https://app.crunch.io/api/datasets/4/variables/gender/"}],',
            '"measures":{"count":{"function":"cube_count","args":[]}}}}}'
        )
    })

    test_that("formulaToSlideQuery helper", {
        # It's just a wrapper with different argument names for internal function
        expect_equal(
            formulaToSlideQuery(~birthyr, ds),
            formulaToCubeQuery(~birthyr, ds)
        )
    })

    test_that("slideQueryEnv helper", {
        expect_equal(
            slideQueryEnv(weight = ds$birthyr),
            list(weight = self(ds$birthyr))
        )
        expect_equal(
            slideQueryEnv(weight = NULL),
            list(weight = list())
        )

        filter <- filters(ds)[["Occasional Political Interest"]]
        expect_equal(
            slideQueryEnv(filter = filter),
            list(filter = list(self(filter)))
        )
        expect_equal(
            slideQueryEnv(filter = NULL),
            list(filter = list())
        )
        expect_equal(
            slideQueryEnv(filter = ds$birthyr < 1980),
            list(filter = list(zcl(ds$birthyr < 1980)))
        )

        expect_equal(
            slideQueryEnv(weight = ds$birthyr, filter = filter),
            list(weight = self(ds$birthyr), filter = list(self(filter)))
        )

        expect_error(
            slideQueryEnv(),
            "Must specify at least one of `weight` or `filter`"
        )
    })
})

test_that("truncateString works", {
    expect_equal(
        truncateString("0123456789", nlines = 4, width = 3),
        "0123456789"
    )

    expect_equal(
        truncateString("0123456789", nlines = 3, width = 3),
        "012345678..."
    )

    expect_equal(
        truncateString("01\n23\n456789", nlines = 3, width = 3),
        "01\n23\n456..."
    )

    expect_equal(
        truncateString("\n\n0123456789", nlines = 4, width = 3),
        "\n\n012345..."
    )

    expect_equal(
        truncateString("0123456789\n", nlines = 4, width = 3),
        "0123456789\n"
    )
})
