context("User access")

user_cat_output <- get_output(data.frame(
    name = c("Bill User", "Roger User"),
    email = c("william.user@example.io", "ruser@crunch.io"),
    teams = NA,
    collaborator = c(FALSE, TRUE)))

with_mock_crunch({
    test_that("users() works with dataset tuples from catalogs", {
        user_cat <- users(datasets()[[1]])

        expect_is(user_cat, "UserCatalog")
        expect_length(user_cat, 2)
        # show method works
        expect_prints(user_cat, user_cat_output)
    })

    test_that("users() works with dataset entities", {
        ds <- loadDataset("ECON.sav")
        user_cat <- users(ds)

        expect_is(user_cat, "UserCatalog")
        expect_length(user_cat, 2)
        # show method works
        expect_prints(user_cat, user_cat_output)
    })

    test_that("users() works with projects", {
        user_cat <- users(projects()[[1]])

        expect_is(user_cat, "UserCatalog")
        expect_length(user_cat, 2)
        # show method works
        expect_prints(user_cat, user_cat_output)
    })
})
