context("User access")

user_cat_output <- get_output(data.frame(
    name = c("Bill User", "Roger User"),
    email = c("william.user@example.io", "ruser@crunch.io"),
    teams = NA,
    collaborator = c(FALSE, TRUE)))

with_mock_crunch({
    ds_cat <- selectDatasetCatalog()[[1]]
    ds <- loadDataset("ECON.sav")

    test_that("users() works with dataset entities from catalogs", {
        user_cat <- users(ds_cat)

        expect_is(user_cat, "UserCatalog")
        expect_length(user_cat, 2)
        # show method works
        expect_output(user_cat, user_cat_output)
    })



    test_that("users() works with dataset entities from catalogs", {
        user_cat <- users(ds)

        expect_is(user_cat, "UserCatalog")
        expect_length(user_cat, 2)
        # show method works
        expect_output(user_cat, user_cat_output)
    })

    test_that("users() works with projects", {
        user_cat <- users(projects()[[1]])

        expect_is(user_cat, "UserCatalog")
        expect_length(user_cat, 2)
        # show method works
        expect_output(user_cat, user_cat_output)
    })


    ### Last accessed functions
    test_that("last_accessed works with dataset entities from catalogs", {
        last_accessed_df <- lastAccessed(ds_cat)

        expect_length(last_accessed_df, 5)
        expect_equal(nrow(last_accessed_df), 2)
        expect_equal(colnames(last_accessed_df),
                     c("name", "email", "access_time", "dataset", "dataset_id"))
    })

    test_that("last_accessed works with dataset objects", {
        last_accessed_df <- lastAccessed(ds)

        expect_length(last_accessed_df, 5)
        expect_equal(nrow(last_accessed_df), 2)
        expect_equal(colnames(last_accessed_df),
                     c("name", "email", "access_time", "dataset", "dataset_id"))
    })


    test_that("last_accessed_for_projects", {
        last_accessed_df <- lastAccessed(projects()[[1]])
        expect_length(last_accessed_df, 5)
        expect_equal(nrow(last_accessed_df), 2)
        expect_equal(colnames(last_accessed_df),
                     c("name", "email", "access_time", "dataset", "dataset_id"))
    })
})
