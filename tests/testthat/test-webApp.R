context("webApp")

with_mock_crunch({
    ds <- loadDataset("test ds")

    test_that("Dataset URLs", {
        with(temp.options(crunch.api="https://fake.crunch.io/api/v2/"), {
            expect_identical(APIToWebURL(ds),
                "https://fake.crunch.io/dataset/511a7c49778030653aab5963")
        })
        expect_identical(webToAPIURL("https://app.crunch.io/dataset/b6c2325a8de9438ebab5d9a42d376b90/browse/eyJhcHBTdGF0ZVN0b3JlIjp0cnVlLCJhbmFseXplIjp7fSwidmFyaWFibGVzTmF2aWdhdG9yIjp7Iml0ZW0iOiIvZWU2NTI0YWFjMzFiNDkyZjk4M2ZiYzM0MGJjODYzYzkvIn19"),
            "https://app.crunch.io/api/datasets/b6c2325a8de9438ebab5d9a42d376b90/")
        expect_identical(webToAPIURL("https://app.crunch.io/dataset/3f57d1924a914176b24969bc6cc9059d?variableId=000194"),
            "https://app.crunch.io/api/datasets/3f57d1924a914176b24969bc6cc9059d/")
        expect_error(webToAPIURL("Not actually a URL"),
            "Not a valid web app URL")
    })
    test_that("Variable URL", {
        expect_identical(APIToWebURL(ds$gender),
            "https://app.crunch.io/dataset/1/browse?variableId=66ae9881e3524f7db84970d556c34552")
    })
    test_that("webApp errors correctly", {
        expect_error(webApp(mtcars), "webApp is not available for objects of class data.frame")
    })
})