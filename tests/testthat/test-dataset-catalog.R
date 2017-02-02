context("Dataset catalog")

with_mock_HTTP({
    cr <- session()
    datcat <- cr$datasets

    test_that("DatasetCatalog instantiates from Shoji", {
        expect_is(datcat, "DatasetCatalog")
    })

    test_that("DatasetCatalog has the right contents", {
        expect_identical(urls(datcat),
            c("api/datasets/3/",
              "api/datasets/2/",
              "api/datasets/1/")) ## C sorting on names
        expect_identical(self(datcat),
            "api/datasets/")
    })

    test_that("active/archived getters", {
        ## NOTE: deferring the "shared" collection
        expect_is(active(datcat), "DatasetCatalog")
        expect_is(archived(datcat), "DatasetCatalog")
        expect_identical(urls(active(datcat)),
            c("api/datasets/3/", "api/datasets/1/"))
        expect_length(active(datcat), 2)
        expect_identical(urls(archived(datcat)),
            "api/datasets/2/")
        expect_length(archived(datcat), 1)
        expect_length(datcat, 3)
        expect_identical(active(archived(datcat)), archived(active(datcat)))
    })

    test_that("Extract methods", {
        expect_is(datcat[["api/datasets/1/"]], "DatasetTuple")
        expect_identical(datcat[["api/datasets/1/"]]@body,
            index(datcat)[["api/datasets/1/"]])
        expect_identical(index(datcat[2:3]), index(datcat)[2:3])
        expect_error(datcat[[500]], "subscript out of bounds")
    })

    test_that("names", {
        expect_identical(names(datcat),
            c("ECON.sav", "an archived dataset", "test ds"))
    })
    test_that("owners", {
        expect_identical(owners(datcat),
            c("api/users/notme/", "api/users/user1/", "api/users/user1/"))
    })
    test_that("ownerNames", {
        expect_identical(ownerNames(datcat),
            c("George", "Fake User", "Fake User"))
    })
    test_that("is.archived", {
        expect_identical(is.archived(datcat), c(FALSE, TRUE, FALSE))
    })
    test_that("is.published/draft", {
        expect_identical(is.published(datcat), c(FALSE, TRUE, TRUE))
        expect_identical(is.draft(datcat), !is.published(datcat))
    })
    test_that("is.archived setter", {
        expect_PATCH(is.archived(datcat[1]) <- TRUE,
            'api/datasets/',
            '{"api/datasets/3/":{"archived":true}}')
        expect_PATCH(archive(datcat[2:3]),
            'api/datasets/',
            '{"api/datasets/1/":{"archived":true}}')
    })
    test_that("is.published setter", {
        expect_PATCH(is.published(datcat[c(1,3)]) <- TRUE,
            'api/datasets/',
            '{"api/datasets/3/":{"is_published":true}}')
        expect_PATCH(publish(datcat[c(1,3)]),
            'api/datasets/',
            '{"api/datasets/3/":{"is_published":true}}')
        expect_PATCH(is.draft(datcat) <- TRUE,
            'api/datasets/',
            '{"api/datasets/2/":{"is_published":false},',
            '"api/datasets/1/":{"is_published":false}}')
    })

    test_that("entity method for tuple", {
        expect_true(is.dataset(entity(datcat[["api/datasets/1/"]])))
    })
})
