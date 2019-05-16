context("Decipher integration")

with_mock_crunch({
    test_that("can authenticate", {
        expect_POST(
            setupDecipherAuth("super_secret_key", "my_company"),
            "https://app.crunch.io/api/integrations/decipher/auth/",
            '{"element":"shoji:entity","body":{"apikey":"super_secret_key",',
            '"company_name":"my_company"}}'
        )
    })

    test_that("can get list of datasets", {
        expect_GET(
            listDecipherSurveys(),
            "https://app.crunch.io/api/users/user1/integrations/decipher_id/surveys"
        )
    })
})

