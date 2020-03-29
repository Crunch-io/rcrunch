context("Authentication")

test_that("login checks for email and password before POSTing", {
    expect_error(
        crunchAuth(email = NULL),
        "Must supply the email address associated with your crunch.io account"
    )
    expect_error(
        crunchAuth(email = 1L, password = NULL),
        "Must supply a password"
    )
})

test_that("without_echo doesn't crash on this OS", {
    without_echo({
        expect_true(TRUE)
    })
})

with_mock_crunch({
    test_that("Jupyter helper sets up env", {
        with(reset.option("crunch.httr_config"), {
            jupyterLogin("test_token")
            cfg <- get_crunch_config()
            expect_identical(cfg$headers[["Authorization"]], "Bearer test_token")
            expect_true(grepl("jupyter.crunch.io", cfg$headers[["user-agent"]]))
            expect_true(grepl("rcrunch", cfg$headers[["user-agent"]]))
        })
    })
})

if (run.integration.tests) {
    with(test_options, {
        test_that("crunchAuth succeeds when it should and not when it shouldn't", {
            logout()
            expect_error(
                crunchAuth("lkjasdfksdfkjhl", password = "w23nrnsod"),
                "Unable to authenticate lkjasdfksdfkjhl"
            )
        })

        test_that("login returns a session object", {
            cr <- suppressMessages(login())
            expect_true(is.list(cr))
            logout()
        })
    })
}
