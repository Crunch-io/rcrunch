context("Dataset catalog")

with_mock_HTTP({
    cr <- session()
    datcat <- cr$datasets

    test_that("DatasetCatalog instantiates from Shoji", {
        expect_is(datcat, "DatasetCatalog")
    })

    test_that("DatasetCatalog has the right contents", {
        expect_identical(urls(datcat),
            c("/api/datasets/dataset3.json",
              "/api/datasets/dataset2.json",
              "/api/datasets/dataset1.json")) ## C sorting on names
        expect_identical(self(datcat),
            "/api/datasets.json")
    })

    test_that("active/archived getters", {
        ## NOTE: deferring the "shared" collection
        expect_is(active(datcat), "DatasetCatalog")
        expect_is(archived(datcat), "DatasetCatalog")
        expect_identical(index(active(datcat)), index(datcat))
        expect_equivalent(index(archived(datcat)), list())
        index(datcat)[[which(names(datcat) == "an archived dataset")]]$archived <- TRUE
        expect_is(active(datcat), "DatasetCatalog")
        expect_is(archived(datcat), "DatasetCatalog")
        expect_identical(urls(active(datcat)),
            c("/api/datasets/dataset3.json", "/api/datasets/dataset1.json"))
        expect_length(active(datcat), 2)
        expect_identical(urls(archived(datcat)),
            "/api/datasets/dataset2.json")
        expect_length(archived(datcat), 1)
        expect_length(datcat, 3)
        expect_identical(active(archived(datcat)), archived(active(datcat)))
    })

    test_that("Extract methods", {
        expect_is(datcat[["/api/datasets/dataset1.json"]], "DatasetTuple")
        expect_identical(datcat[["/api/datasets/dataset1.json"]]@body,
            index(datcat)[["/api/datasets/dataset1.json"]])
        expect_identical(index(datcat[2:3]), index(datcat)[2:3])
        expect_error(datcat[[500]], "subscript out of bounds")
    })

    test_that("names", {
        expect_identical(names(datcat),
            c("ECON.sav", "an archived dataset", "test ds"))
    })
    test_that("owners", {
        expect_identical(owners(datcat),
            c("/api/users/notme.json", "/api/users/user1.json", "/api/users/user1.json"))
    })
    test_that("ownerNames", {
        expect_identical(ownerNames(datcat),
            c("George", "Fake User", "Fake User"))
    })
    test_that("is.archived", {
        expect_identical(is.archived(datcat), rep(FALSE, 3))
    })
    test_that("is.published/draft", {
        expect_identical(is.published(datcat), c(FALSE, TRUE, TRUE))
        expect_identical(is.draft(datcat), !is.published(datcat))
    })
    test_that("is.archived setter", {
        expect_error(is.archived(datcat[1]) <- TRUE,
            paste0('PATCH /api/datasets.json {"/api/datasets/dataset3.json":',
                    '{"archived":true}}'),
            fixed=TRUE
        )
        expect_error(archive(datcat[2:3]),
            paste0('PATCH /api/datasets.json {',
                    '"/api/datasets/dataset2.json":{"archived":true},',
                    '"/api/datasets/dataset1.json":{"archived":true}}'),
            fixed=TRUE
        )
    })
    test_that("is.published setter", {
        expect_error(is.published(datcat[c(1,3)]) <- TRUE,
            paste0('PATCH /api/datasets.json {"/api/datasets/dataset3.json":',
                    '{"is_published":true}}'),
            fixed=TRUE
        )
        expect_error(publish(datcat[c(1,3)]),
            paste0('PATCH /api/datasets.json {"/api/datasets/dataset3.json":',
                    '{"is_published":true}}'),
            fixed=TRUE
        )
        expect_error(is.draft(datcat) <- TRUE,
            paste0('PATCH /api/datasets.json {',
                    '"/api/datasets/dataset2.json":{"is_published":false},',
                    '"/api/datasets/dataset1.json":{"is_published":false}}'),
            fixed=TRUE
        )
    })

    test_that("entity method for tuple", {
        expect_true(is.dataset(entity(datcat[["/api/datasets/dataset1.json"]])))
    })
})
