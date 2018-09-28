context("Folder methods for projects with the new API")

with_mock_crunch({
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

    test_that("folder() and rootFolder() for projects", {
        expect_null(folder(projects()))
        expect_identical(folder(cd(projects(), "Project One")), projects())
        expect_identical(folder(cd(projects(), "Project One/Project Two")),
            cd(projects(), "Project One"))
        expect_identical(rootFolder(cd(projects(), "Project One/Project Two")),
            projects())
    })

    test_that("path()", {
        expect_identical(path(projects()), "/")
        expect_identical(path(cd(projects(), "Project One")), "/Project One")
        expect_identical(path(cd(projects(), "Project One/Project Two")),
            "/Project One/Project Two")
    })

    create_group_2 <- '{"element":"shoji:entity","body":{"name":"Group 2"}}'
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

    move_testds <- paste0(
        '{"element":"shoji:catalog",',
        '"index":{"https://app.crunch.io/api/datasets/1/":{}},"graph":[',
        '"https://app.crunch.io/api/datasets/3/",',
        '"https://app.crunch.io/api/datasets/1streaming/",',
        '"https://app.crunch.io/api/datasets/1/"]}'
    )
    test_that("mv dataset", {
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
    test_that("Legacy methods: datasets<-", {
        expect_PATCH(
            datasets(proj[["Project Two"]]) <- ds,
            "https://app.crunch.io/api/projects/project2/",
            move_testds
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
        expect_PATCH(
            projects() %>%
                cd("Project One") %>%
                mv("Project Two", "Project Five"),
            "https://app.crunch.io/api/projects/project5/",
            '{"element":"shoji:catalog",',
            '"index":{"https://app.crunch.io/api/projects/project2/":{}},',
            '"graph":["https://app.crunch.io/api/projects/project2/"]}'

        )
    })

    test_that("can't move datasets to root", {
        expect_error(
            proj %>%
                mv(ds, "/"),
            "Can't move a dataset to the top-level project"
        )
    })

    test_that("mv error handling", {
        expect_error(
            proj %>%
                mv("NOT A GROUP", "Project Two"),
            "Undefined elements selected: NOT A GROUP"
        )
    })

    test_that("rmdir an empty project", {
        expect_error(
            proj %>% rmdir("Project Five"),
            "Must confirm deleting folder" # TODO: should error say "project"?
        )
        with_consent({
            expect_DELETE(
                proj %>% rmdir("Project Five"),
                "https://app.crunch.io/api/projects/project5/"
            )
            expect_DELETE(
                projects() %>% rmdir("Project One/Project Five"),
                "https://app.crunch.io/api/projects/project5/"
            )
            expect_DELETE(
                projects() %>%
                    cd("Project One/Project Two") %>%
                    rmdir("../Project Five"),
                "https://app.crunch.io/api/projects/project5/"
            )
        })
    })
    test_that("rmdir a non-empty project errors", {
        with_consent({
            expect_error(
                proj %>%
                    rmdir("Project Two"),
                "Cannot remove 'Project Two' because it is not empty. Move its contents somewhere else and then retry."
            )
        })
    })
    test_that("Can't rmdir the root", {
        expect_error(
            ds %>% rmdir("/"),
            "Cannot delete root folder"
        )
    })

    test_that("rmdir error handling", {
        expect_error(
            proj %>%
                rmdir("NOT A GROUP"),
            '"NOT A GROUP" is not a folder'
        )
    })

    test_that("members() methods work on ProjectFolders", {
        expect_is(members(proj), "MemberCatalog")
        expect_PATCH(members(proj) <- "me@myself.io",
            self(members(proj)))
    })

    test_that("print project folders", {
        with(temp.option(crayon.enabled = FALSE), {
            ## Coloring aside, the default print method should look like you
            ## printed the vector of names (plus the path printed above)
            expect_output(
                print(projects()),
                capture.output(print(names(projects()))),
                fixed = TRUE
            )
            expect_output(
                print(cd(projects(), "Project Three")),
                "project(0)",
                fixed = TRUE
            )
            ## These are obfuscated because of archaic restrictions on UTF-8
            skip_on_cran()
            source("print-projects.R", local = TRUE)
        })
    })
})
