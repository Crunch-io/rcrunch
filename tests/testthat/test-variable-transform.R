context("Insertions")

insrt <- Insertion(anchor = 6, name = "Low", `function` = list(combine = c(1, 2)))

with_mock_crunch({
    ds <- loadDataset("test ds")
    test_that("transformations post", {
        expect_null(transforms(ds$gender))
        expect_PATCH(transforms(ds$gender) <- insrt,
                     'https://app.crunch.io/api/datasets/1/variables/gender/',
                     '{"element":"shoji:entity","body":{"view":{"transforms"',
                     ':{"insertions":{"anchor":6,"name":"Low","function":',
                     '{"combine":[1,2]}}}}}}')
    })

})
