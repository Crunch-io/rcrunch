context("Embed preparation")

with_mock_HTTP({
    ds1 <- loadDataset("test ds")
    test_that("preEmbedCheck does not error", {
        expect_output(preEmbedCheck(ds1),
            "We recommend using only categorical and multiple_response variables. These 4 variables are not")
    })
})
