context("Folder methods for projects with the new API")

with_mock_crunch({
    .mockPaths("alt")
    clearCache() # Make sure we read from our alternate fixtures
    on.exit(clearCache())

    test_that("The feature flag is 'on'", {
        expect_false(featureFlag("old_projects_order"))
    })

    proj <- projects()[["Project One"]]

    test_that("Root and cd()", {
        expect_true(is.project(projects()))
        expect_true(is.project(cd(projects(), "Project One")))
        expect_identical(cd(projects(), "Project One"), proj)
        expect_true(is.project(cd(projects(), "Project One/Project Two")))
    })

    test_that("mkdir with nested project", {
        payload <- '{"element":"shoji:catalog","body":{"name":"Group 2"}}'
        expect_POST(
            proj %>%
                mkdir("Group 2"),
            "https://app.crunch.io/api/projects/project1/",
            payload
        )
        expect_POST(
            projects() %>%
                mkdir("Project One/Group 2"),
            "https://app.crunch.io/api/projects/project1/",
            payload
        )
        expect_POST(
            projects() %>%
                mkdir("Project One/Project Two/Group 2"),
            "https://app.crunch.io/api/projects/project2/",
            payload
        )
    })
})
