context("Folder methods for dataset organization")

with_mock_crunch({
    proj <- projects()[["Project One"]]
    ds <- loadDataset("ECON.sav")
    other_ds <- loadDataset("test ds")
    test_that("Starting datasets order", {
        expect_prints(ordering(proj),
            paste(
                "[+] Group 1",
                "    ECON.sav",
                sep = "\n"
            ),
            fixed = TRUE
        )
    })

    test_that("print method on a project", {
        expect_prints(proj,
            paste(
                paste0("Project ", dQuote("Project One")),
                "",
                "[+] Group 1",
                "    ECON.sav",
                sep = "\n"
            ),
            fixed = TRUE
        )
    })

    test_that("mkdir with dataset order", {
        expect_PUT(
            proj %>%
                mkdir("Group 2"),
            "https://app.crunch.io/api/projects/project1/datasets/order/",
            '{"graph":[{"Group 1":["https://app.crunch.io/api/datasets/3/"]},',
            '{"Group 2":[]}]}'
        )
    })
    test_that("mkdir with long path", {
        expect_PUT(
            proj %>%
                mkdir("Group 1/Nested/further"),
            "https://app.crunch.io/api/projects/project1/datasets/order/",
            '{"graph":[{"Group 1":["https://app.crunch.io/api/datasets/3/",',
            '{"Nested":[{"further":[]}]}]}]}'
        )
        expect_PUT(
            proj %>%
                mkdir("Group 2/Nested"),
            "https://app.crunch.io/api/projects/project1/datasets/order/",
            '{"graph":[{"Group 1":["https://app.crunch.io/api/datasets/3/"]},',
            '{"Group 2":[{"Nested":[]}]}]}'
        )
    })

    test_that("mv a dataset elsewhere in the order", {
        expect_PUT(
            proj %>%
                mv(ds, "Group 2"),
            "https://app.crunch.io/api/projects/project1/datasets/order/",
            '{"graph":[{"Group 1":[]},',
            '{"Group 2":["https://app.crunch.io/api/datasets/3/"]}]}'
        )
    })
    test_that("mv a dataset elsewhere in the order, nested", {
        expect_PUT(
            proj %>%
                mv(ds, "Group 2/Nested"),
            "https://app.crunch.io/api/projects/project1/datasets/order/",
            '{"graph":[{"Group 1":[]},',
            '{"Group 2":[{"Nested":["https://app.crunch.io/api/datasets/3/"]}]}]}'
        )
    })
    test_that("mv a dataset to the root", {
        expect_PUT(
            proj %>%
                mv(ds, "/"),
            "https://app.crunch.io/api/projects/project1/datasets/order/",
            '{"graph":[{"Group 1":[]},"https://app.crunch.io/api/datasets/3/"]}'
        )
    })
    test_that("mv a dataset with absolute path", {
        expect_PUT(
            proj %>%
                mv(ds, "/Group 2/Nested"),
            "https://app.crunch.io/api/projects/project1/datasets/order/",
            '{"graph":[{"Group 1":[]},',
            '{"Group 2":[{"Nested":["https://app.crunch.io/api/datasets/3/"]}]}]}'
        )
    })

    test_that("Adding a dataset to a project by mv", {
        expect_PATCH(
            proj %>%
                mv(other_ds, "Group 1"),
            "https://app.crunch.io/api/datasets/1/",
            '{"owner":"https://app.crunch.io/api/projects/project1/"}'
        )
    })

    test_that("mv a group that already exists", {
        expect_PUT(
            proj %>%
                mv("Group 1", "Top level"),
            "https://app.crunch.io/api/projects/project1/datasets/order/",
            '{"graph":[{"Top level":[{"Group 1":',
            '["https://app.crunch.io/api/datasets/3/"]}]}]}'
        )
    })

    test_that("mv error handling", {
        expect_error(
            proj %>%
                mv("NOT A GROUP", "Group 2"),
            "Undefined groups selected: NOT A GROUP"
        )
    })

    test_that("rmdir if the dir is empty", {
        ## TODO: make a more complex project2
    })

    test_that("rmdir if not empty", {
        expect_error(
            proj %>%
                rmdir("Group 1"),
            "Cannot remove 'Group 1' because it is not empty. Move its contents somewhere else and then retry."
        )
    })

    test_that("rmdir error handling", {
        expect_error(
            proj %>%
                rmdir("NOT A GROUP"),
            ## This is actually ok
            NA
        )
    })
})

with_test_authentication({
    ds1 <- createDataset(name = "One")
    ds2 <- createDataset(name = "Two")
    proj <- newProject(name = now())

    proj %>%
        mv(ds1, "/")
    test_that("Can move dataset into project root", {
        expect_identical(urls(datasets(proj)), self(ds1))
        expect_identical(urls(ordering(proj)), self(ds1))
    })

    proj %>%
        mkdir("A group/Within another") %>%
        mv(ds2, "A group")
    test_that("Can move datasets into a project in a folder", {
        expect_true(setequal(urls(datasets(proj)), c(self(ds1), self(ds2))))
        expect_true(setequal(urls(ordering(proj)), c(self(ds1), self(ds2))))
        expect_prints(
            ordering(proj),
            paste(
                "One",
                "[+] A group",
                "    [+] Within another",
                "        (Empty group)",
                "    Two",
                sep = "\n"
            )
        )
    })
})
