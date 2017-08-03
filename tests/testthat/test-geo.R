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
    
    test_that("if there is no geography on a variable, geo() returns null", {
        expect_null(geo(ds$gender))
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
})

with_test_authentication({
    # TODO: create a variable with will match more than one geo and test the error works.
    # TODO: create a text variable that matches geo to ensure that works
})