context("Sessions")

with_mock_crunch({
    cr <- session()
    test_that("session() returns a session object", {
        expect_is(cr$datasets, "DatasetCatalog")
        expect_is(cr[["projects"]], "ProjectCatalog")
    })

    test_that("Can assign into a session object", {
        cr$datasets <- cr$datasets[1]
        expect_is(cr$datasets, "DatasetCatalog")
    })

    test_that("Invalid session attributes", {
        expect_error(cr[[4]], "Unknown session attribute: 4")
        expect_error(cr$NOTACATALOG, "Unknown session attribute: NOTACATALOG")
        expect_error(cr[[4]] <- "x", "Unknown session attribute: 4")
    })
})
