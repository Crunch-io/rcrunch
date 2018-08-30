context("Folder methods for projects with the new API")

with_mock_crunch({
    .mockPaths("alt")
    clearCache() # Make sure we read from our alternate fixtures
    on.exit(clearCache())

    test_that("The feature flag is 'on'", {
        expect_false(featureFlag("old_projects_order"))
    })

    proj <- projects()[["Project One"]]
    ds <- loadDataset("test ds")

    test_that("Root and cd()", {
        expect_true(is.project(projects()))
        expect_true(is.project(cd(projects(), "Project One")))
        expect_identical(cd(projects(), "Project One"), proj)
        expect_true(is.project(cd(projects(), "Project One/Project Two")))
    })

    test_that("cd ..", {
        expect_identical(
            projects() %>% cd("Project One/Project Two") %>% cd(".."),
            cd(projects(), "Project One")
        )
    })

    test_that("cd /", {
        expect_identical(
            projects() %>% cd("Project One/Project Two") %>% cd("/"),
            projects()
        )
        expect_identical(
            projects() %>% cd("Project One/Project Two") %>% cd("/Project One"),
            cd(projects(), "Project One")
        )
    })

    create_group_2 <- '{"element":"shoji:catalog","body":{"name":"Group 2"}}'
    test_that("mkdir on root", {
        expect_POST(
            projects() %>% mkdir("Group 2"),
            "https://app.crunch.io/api/projects/",
            create_group_2
        )
    })

    test_that("mkdir with nested project", {
        expect_POST(
            proj %>%
                mkdir("Group 2"),
            "https://app.crunch.io/api/projects/project1/",
            create_group_2
        )
        expect_POST(
            projects() %>%
                mkdir("Project One/Group 2"),
            "https://app.crunch.io/api/projects/project1/",
            create_group_2
        )
        expect_POST(
            projects() %>%
                mkdir("Project One/Project Two/Group 2"),
            "https://app.crunch.io/api/projects/project2/",
            create_group_2
        )
    })

    test_that("mv dataset", {
        move_testds <- paste0(
            '{"element":"shoji:catalog",',
            '"index":{"https://app.crunch.io/api/datasets/1/":{}},"graph":[',
            '"https://app.crunch.io/api/datasets/3/",',
            '"https://app.crunch.io/api/datasets/1streaming/",',
            '"https://app.crunch.io/api/datasets/1/"]}'
        )
        expect_PATCH(
            projects() %>%
                cd("Project One") %>%
                mv("test ds", "Project Two"),
            "https://app.crunch.io/api/projects/project2/",
            move_testds
        )
        expect_PATCH(
            projects() %>%
                mv(ds, "Project One/Project Two"),
            "https://app.crunch.io/api/projects/project2/",
            move_testds
        )
        expect_no_request(
            # That's already where it is
            projects() %>%
                mv(ds, "Project One")
        )
    })

    test_that("mv several datasets (with magic select fn)", {
        expect_PATCH(
            projects() %>%
                cd("Project One") %>%
                mv(contains("a"), "Project Two"),
            "https://app.crunch.io/api/projects/project2/",
            '{"element":"shoji:catalog",',
            '"index":{"https://app.crunch.io/api/datasets/2/":{},',
            '"https://app.crunch.io/api/datasets/streaming-no-msg/":{}},"graph":[',
            '"https://app.crunch.io/api/datasets/3/",',
            '"https://app.crunch.io/api/datasets/1streaming/",',
            '"https://app.crunch.io/api/datasets/2/",',
            '"https://app.crunch.io/api/datasets/streaming-no-msg/"]}'
        )
    })

    test_that("mv dataset creates a project if necessary", {
        expect_POST(
            projects() %>%
                cd("Project One") %>%
                mv("test ds", "Group 2"),
            "https://app.crunch.io/api/projects/project1/",
            create_group_2
        )
    })

    test_that("mv project", {

    })

    test_that("can't move anything to root", {
        skip("TODO")
    })

    test_that("mv error handling", {

    })

    test_that("rmdir an empty project", {

    })
    test_that("rmdir a non-empty project errors", {

    })

    test_that("rmdir error handling", {

    })
})
