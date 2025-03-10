context("Retrieving dataset list and single datasets")

with_mock_crunch({
    test_that("listDatasets lists datasets in your personal project (and warns once)", {
        options(crunch.list.personal.msg = NULL)
        expect_warning(
            expect_identical(listDatasets(), c(
                "test ds",
                "streaming no messages"
            )),
            paste(
                "As of crunch 1.26.0, listDatasets() with no project specified",
                "only lists your 'personal' datasets (those that you created)",
                "and not those that were shared with you."
            ),
            fixed = TRUE
        )
        # Also test that this only warns the first time
        expect_warning(
            expect_identical(
                listDatasets("archived"),
                "an archived dataset"
            ),
            NA
        )
        expect_identical(listDatasets("all"), c(
            "an archived dataset",
            "test ds",
            "streaming no messages"
        ))
    })
    test_that("listDatasets in project", {
        expect_identical(
            listDatasets(project = "Project One"),
            c(
                "test ds",
                "streaming no messages"
            )
        )
        expect_identical(
            listDatasets("all", project = "Project One"),
            c(
                "an archived dataset",
                "test ds",
                "streaming no messages"
            )
        )
        expect_identical(
            listDatasets(project = "Project One/Project Two"),
            c(
                "ECON.sav",
                "streaming test ds"
            )
        )
    })
    test_that("listDatasets(refresh=TRUE) drops caches", {
        with(temp.option(httpcache.log = ""), {
            logs <- capture.output({
                listDatasets(refresh = TRUE)
            })
        })
        in_logs <- function(str, loglines) {
            any(grepl(str, loglines, fixed = TRUE))
        }
        expect_true(in_logs("CACHE DROP ^https://app[.]crunch[.]io/api/datasets/by_name/", logs))
    })

    test_that("listDatasets error handling", {
        expect_error(
            listDatasets(project = 42),
            "Project 42 is not valid"
        )
    })

    test_that("loadDataset loads", {
        ds <- loadDataset("test ds", project = "Project One")
        expect_true(is.dataset(ds))
        expect_identical(name(ds), "test ds")
    })

    test_that("loadDataset by index is deprecated", {
        expect_deprecated(ds <- loadDataset(1, project = "Project One"))
        expect_true(is.dataset(ds))
        expect_identical(name(ds), "test ds")
        expect_error(
            expect_deprecated(
                loadDataset(666)
            ),
            "subscript out of bounds"
        )
    })

    test_that("loadDataset loads with DatasetTuple", {
        # Is this a use case we need to support?
        cr <- session()
        ds <- loadDataset(cr$datasets[["test ds"]])
        expect_true(is.dataset(ds))
        expect_identical(name(ds), "test ds")
        expect_true(is.dataset(loadDataset(cr$datasets$`test ds`)))
    })

    test_that("loadDataset by URL when in main catalog", {
        ds <- loadDataset("test ds", project = "Project One")
        ds_by_url <- loadDataset(self(ds))
        expect_is(ds_by_url, "CrunchDataset")
        expect_identical(name(ds), name(ds_by_url))
        expect_identical(self(ds), self(ds_by_url))
    })
    test_that("loadDataset by URL when not in main catalog", {
        ds2 <- loadDataset("https://app.crunch.io/api/datasets/four/")
        expect_is(ds2, "CrunchDataset")
        expect_identical(
            self(ds2),
            "https://app.crunch.io/api/datasets/four/"
        )
        expect_identical(name(ds2), "mtcars from R")
    })

    test_that("loadDataset by web app URL", {
        ds3 <- loadDataset("https://app.crunch.io/dataset/3/browse")
        expect_is(ds3, "CrunchDataset")
        expect_identical(name(ds3), "ECON.sav")
    })

    test_that("loadDataset by path", {
        ds3 <- loadDataset("Project One/Project Two/ECON.sav")
        expect_is(ds3, "CrunchDataset")
        expect_identical(name(ds3), "ECON.sav")
        ds1 <- loadDataset("~/test ds") # nolint
        expect_is(ds1, "CrunchDataset")
        expect_identical(name(ds1), "test ds")
    })

    test_that("loadDataset(refresh=TRUE) drops caches", {
        with(temp.option(httpcache.log = ""), {
            logs <- capture.output({
                loadDataset("test ds", refresh = TRUE, project = "Project One")
            })
        })
        in_logs <- function(str, loglines) {
            any(grepl(str, loglines, fixed = TRUE))
        }
        expect_true(in_logs("CACHE DROP ^https://app[.]crunch[.]io/api/projects/", logs))
    })

    test_that("loadDataset error handling", {
        options(find.dataset.no.project = NULL)
        expect_warning(
            expect_error(
                loadDataset("not a dataset"),
                paste(dQuote("not a dataset"), "not found")
            ),
            "Finding datasets by name without specifying a path is no longer supported"
        )
        expect_error(
            loadDataset("not a dataset", project = 42),
            "project must be a `CrunchProject` object, a URL, or a path to a project from the root"
        )
        expect_error(
            loadDataset(c("test ds", "ECON.sav")),
            # Not the clearest error message, but does signal something wrong with input
            '"test ds" is not a folder'
        )
        expect_error(
            loadDataset(NULL),
            paste0(
                "'dataset' should be a character dataset name, path, or URL, ",
                "not an object of class NULL"
            )
        )
        expect_error(
            loadDataset(list(5)),
            paste0(
                "'dataset' should be a character dataset name, path, or URL, ",
                "not an object of class list"
            )
        )
    })
})
