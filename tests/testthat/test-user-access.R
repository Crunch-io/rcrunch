context("User access")

with_mock_crunch({

    test_that("last_accessed works with dataset entities from catalogs", {
        # grab the econ.sav entity from the dataset catalog
        ds_cat <- selectDatasetCatalog()[[1]]

        last_accessed_df <- userInfo(ds_cat)

        expect_length(last_accessed_df, 5)
        expect_equal(nrow(last_accessed_df), 2)
        expect_equal(colnames(last_accessed_df),
                     c("name", "email", "access_time", "dataset", "dataset_id"))

    })

    test_that("last_accessed works with dataset objects", {
        ds <- loadDataset("ECON.sav")

        last_accessed_df <- userInfo(ds)

        expect_length(last_accessed_df, 5)
        expect_equal(nrow(last_accessed_df), 2)
        expect_equal(colnames(last_accessed_df),
                     c("name", "email", "access_time", "dataset", "dataset_id"))

    })

    test_that("last_accessed_for_projects", {
        last_accessed_df <- projectUserInfo("Project One")
        expect_length(last_accessed_df, 5)
        expect_equal(nrow(last_accessed_df), 2)
        expect_equal(colnames(last_accessed_df),
                     c("name", "email", "access_time", "dataset", "dataset_id"))
    })
})
