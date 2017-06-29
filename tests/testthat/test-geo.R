context("Various geo functions")

with_mock_crunch({
    ds <- loadDataset("test ds")
    geo_data <- geo(ds$location)
    test_that("geo getter", {
        expect_equal(geo_data$feature_key, "properties.location")
        expect_equal(geo_data$match_field, "name")
        expect_is(geo_data$geodatum, "CrunchGeodata")
        expect_equal(geo_data$geodatum$name, "GB Regions")
        expect_equal(geo_data$geodatum$location, "https://s.crunch.io/some/wrong/gb_eer_doesnotexist.topojson")
    })
    test_that("geo error", {
        expect_null(geo(ds$gender))
    })

    test_that("geo setter", {
        expect_PATCH(geo(ds$location)$feature_key <- "properties.location2",
                     'https://app.crunch.io/api/datasets/1/variables/location/',
                     '{"view":{"geodata":[{"geodatum":"https://app.crunch.io/api/geodata/8684c65ff11c4cc3b945c0cf1c9b2a7f/"',
                     ',"feature_key":"properties.location2","match_field":"name"}]}}'
                     )
        expect_PATCH(geo(ds$location)$match_field <- "name2",
                     'https://app.crunch.io/api/datasets/1/variables/location/',
                     '{"view":{"geodata":[{"geodatum":"https://app.crunch.io/api/geodata/8684c65ff11c4cc3b945c0cf1c9b2a7f/"',
                     ',"feature_key":"properties.location","match_field":"name2"}]}}'
                     )
        expect_PATCH(geo(ds$location)$geodatum <- "https://app.crunch.io/api/geodata/newone/",
                     'https://app.crunch.io/api/datasets/1/variables/location/',
                     '{"view":{"geodata":[{"geodatum":"https://app.crunch.io/api/geodata/newone/"',
                     ',"feature_key":"properties.location","match_field":"name"}]}}'
        )
    })

    test_that("fetchGeoFile", {
        test_crGeo <- CrunchGeography(
            geodatum = CrunchGeodata(crGET("https://app.crunch.io/api/geodata/newone/")),
            feature_key = "none",
            match_field = "none")
        # topojson_read() doesn't result in a get because it reads directly through readOGR in the geojsonio
        expect_error(fetchGeoFile(geo_data),
                   'Cannot open data source')
        expect_GET(fetchGeoFile(test_crGeo),
            'https://s.crunch.io/some/wrong/path.geojson')
        test_crGeo$geodatum$location <- "https://notajsonatall.nope"
        expect_error(fetchGeoFile(test_crGeo), "Unknown filetype ", dQuote("nope"), " in geodata url: ", "https://notajsonatall.nope")
    })
})
