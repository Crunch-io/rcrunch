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
                sep="\n"),
            fixed=TRUE)
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
})
