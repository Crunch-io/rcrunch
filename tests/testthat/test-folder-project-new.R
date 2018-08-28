context("Folder methods for projects with the new API")

with_mock_crunch({
    .mockPaths("alt")
    clearCache() # Make sure we read from our alternate fixtures
    on.exit(clearCache())

    test_that("The feature flag is 'on'", {
        expect_false(featureFlag("old_projects_order"))
    })
})
