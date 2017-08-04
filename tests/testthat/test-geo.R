context("Various geo functions")

with_mock_crunch({
    ds <- loadDataset("test ds")
    geo_data <- geo(ds$location)
    test_that("geo getter", {
        expect_is(geo_data, "CrunchGeography")
        expect_equal(geo_data$feature_key, "properties.location")
        expect_equal(geo_data$match_field, "name")
        expect_equal(geo_data$geodatum, 
            "https://app.crunch.io/api/geodata/8684c65ff11c4cc3b945c0cf1c9b2a7f/")
    })
    
    test_that("is.Geodata", {
        expect_true(is.Geodata(Geodata(crGET("https://app.crunch.io/api/geodata/8684c65ff11c4cc3b945c0cf1c9b2a7f/"))))
    })
    
    test_that("if there is no geography on a variable, geo() returns null", {
        expect_null(geo(ds$gender))
    })
    
    test_that("can remove a geo association", {
        expect_PATCH(geo(ds$location) <- NULL,
                     'https://app.crunch.io/api/datasets/1/variables/location/',
                     '{"element":"shoji:entity","body":{"view":{"geodata":[]}}}')
    })

    test_that("we can update individual fiels of the geography", {
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
        # geographies can also be set by assigning a CrunchGeography to a variable
        expect_PATCH(ds$gender <- geo_data,
                     'https://app.crunch.io/api/datasets/1/variables/gender/',
                     '{"view":{"geodata":[{"geodatum":"https://app.crunch.io/api/geodata/8684c65ff11c4cc3b945c0cf1c9b2a7f/"',
                     ',"feature_key":"properties.location","match_field":"name"}]}}'
        )
    })
    
    avail_features <- availableGeodataFeatures()
    guesses <- c("foo", "bar", "Scotland", "North", "Midlands", "London")
    test_that("availableFeatures", {
        expect_is(avail_features, "data.frame")
        expect_equal(dim(avail_features), c(14, 6))
        expect_equal(as.character(avail_features$value),
                     c("UKH", "UKI", "UKL", "UKF", "UKJ", "UKC", "East",
                       "London", "Wales", "Scotland", "Northern Ireland",
                       "Midlands", "South", "North"))
        
    })
    test_that("scoreCatToFeat", {
        # check the score for guesses and mock available features is accurate
        features <- subset(avail_features, property == "name")$value
        expect_equal(scoreCatToFeat(features, guesses), 0.4)
    })
    test_that("matchCatToFeat", {
        # There should be only one match, and it should be on property.name
        matches <- matchCatToFeat(guesses, all_features = avail_features)
        expect_is(matches, "data.frame")
        expect_equal(nrow(matches), 1)
        expect_equal(matches$value, 0.4)
        expect_equal(as.character(matches$property), "name")
    })
    
    test_that("addGeoMetadata", {
        # full run of adding geometadata including guessing the geodata to use
        geo_to_add <- addGeoMetadata(ds$location)
        expect_is(geo_to_add, "CrunchGeography")
        expect_equal(geo_to_add$feature_key, "properties.name")
        expect_equal(geo_to_add$match_field, "name")
        expect_equal(geo_to_add$geodatum,
        "https://app.crunch.io/api/geodata/8684c65ff11c4cc3b945c0cf1c9b2a7f/")
    })

    test_that("addGeoMetadata input validation", {
        # adding dQuote("ds$not_a_var") to error string inexplicably doesn't
        # match with testthat
        expect_error(addGeoMetadata(ds$not_a_var),
                     ".* must be a Crunch Variable.")
        expect_error(addGeoMetadata(ds$starttime),
             "The variable ", dQuote("ds$starttime"),
             " is neither a categorical or text variable.")
        expect_error(addGeoMetadata(ds$gender), 
             "None of the geographies match at all. Either the variable is",
             " wrong, or Crunch doesn't yet have geodata for this variable.")
    })
    
    test_that("CrunchGeography show methods", {
        expect_output(geo_data,
                     "geodatum name: 		GB Regions\n",
                     "geodatum tdescription: 	These are the GB regions\n",
                     "geodatum url: 		https://app.crunch.io/api/geodata/8684c65ff11c4cc3b945c0cf1c9b2a7f/\n",
                     "feature_key: 		properties.location\n",
                     "match_field: 		name")
    })
    
    test_that("Geodata methods", {
        gd <- Geodata(crGET(geo_data$geodatum))
        expect_is(gd, "Geodata")
        expect_equal(name(gd), "GB Regions")
        expect_equal(description(gd), "These are the GB regions")
    })
})

with_test_authentication({
    # # setup a geography because one doesn't already exits locally
    # # TODO: only trigger locally, but check to make sure they haven't already been created.
    # # test01/stable already has these
    # payload <- list("description" = "use properties.name or properties.postal-code",
    #                 "format" = "geojson",
    #                 "name" = "US States",
    #                 "location" = "https://s.crunch.io/geodata/leafletjs/us-states.geojson",
    #                 "owner_id" = "00002")
    # crPOST("http://local.crunch.io:8080/api/geodata/", body=toJSON(payload))
    # # setup a second geography to test erroring when multiple match
    # payload <- list("description" = "properties.name or properties.postal-code",
    #                 "format" = "topojson",
    #                 "name" = "US States Topojson",
    #                 "location" = "https://s.crunch.io/geodata/leafletjs/us-states.topojson",
    #                 "owner_id" = "00002")
    # crPOST("http://local.crunch.io:8080/api/geodata/", body=toJSON(payload))
    # # setup a second geography to test erroring when multiple match
    # payload <- list("description" = "Census regions",
    #                 "format" = "topojson",
    #                 "name" = "US Census Regions",
    #                 "location" = "https://s.crunch.io/geodata/crunch-io/cb_2015_us_region_20m.topojson",
    #                 "owner_id" = "00002")
    # crPOST("http://local.crunch.io:8080/api/geodata/", body=toJSON(payload))
        
    geo_ds <- newDataset(df)
    test_that("Can match and set geodata on a text variable", {
        geo_ds$region <- rep(c("South", "West", "West", "South", "West"), 4)
        expect_silent(geo_ds$region <- addGeoMetadata(geo_ds$region))
        expect_output(geo(geo_ds$region),
                      "geodatum name: 		US Census Regions\n",
                      "geodatum description: 	Census regions\n",
                      "geodatum url: 		.*\n", # the geodatum id will change, so the url will change.
                      "feature_key: 		properties.name\n",
                      "match_field: 		name")
    })
    
    test_that("Can match and set geodata on a categorical variable", {
        geo_ds$region2 <- factor(rep(c("South", "West", "West", "South", "West"), 4))
        expect_silent(geo_ds$region2 <- addGeoMetadata(geo_ds$region2))
        expect_output(geo(geo_ds$region2),
                      "geodatum name: 		US Census Regions\n",
                      "geodatum description: 	Census regions\n",
                      "geodatum url: 		.*\n", # the geodatum id will change, so the url will change.
                      "feature_key: 		properties.name\n",
                      "match_field: 		name")
    })
    
    geo_ds$state <- rep(c("Alabama", "Alaska", "Arizona", "Arkansas", "California"), 4)
    test_that("There is an error if more than one geography matches", {
        expect_error(geo_ds$state <- addGeoMetadata(geo_ds$state),
                     "There is more than one possible match. Please specify the geography manually.*")
    })
    test_that("can manually set a geography after a failed match", {
        avail_geo <- availableGeodata()
        new_geo <- CrunchGeography(geodatum = urls(avail_geo["US States Topojson"]),
                                        feature_key = "name",
                                        match_field = "name")
        geo_ds$state <- new_geo
        expect_is(geo(geo_ds$state), "CrunchGeography")
        expect_identical(geo(geo_ds$state), new_geo)
    })
    
    test_that("can remove a geo association", {
        expect_silent(geo(geo_ds$state) <- NULL)
        expect_null(geo(geo_ds$state))
    })
})