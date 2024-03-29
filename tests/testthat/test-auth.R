context("Authentication")


test_that("without_echo doesn't crash on this OS", {
    without_echo({
        expect_true(TRUE)
    })
})


test_that("sitrep works", {
    with(temp.option(crunch = list(
        crunch.api = "https://app.crunch.io/api/", crunch.api.key = "key12324567890---")
    ), {
        expect_message(
            crunch_sitrep(),
            paste0(
                "crunch API situation report\n",
                "API: https://app.crunch.io/api/\n",
                "     (set using `set_crunch_opts(crunch.api = ...)`)\n",
                "key: key123245********\n",
                "     (set using `set_crunch_opts(crunch.api.key = ...)`)"
            ),
            fixed = TRUE
        )
    })

    with(temp.option(crunch = list(crunch.api = "url", crunch.api.key = "key12324567890---")), {
      expect_message(
        crunch_sitrep(),
        paste0(
          "crunch API situation report\n",
          "API: url\n",
          "     WARNING! API not in expected form: https://xyz.crunch.io/api/\n",
          "     (set using `set_crunch_opts(crunch.api = ...)`)\n",
          "key: key123245********\n",
          "     (set using `set_crunch_opts(crunch.api.key = ...)`)"
        ),
        fixed = TRUE
      )
    })
})


test_that("key redacting works", {
    expect_equal(redact_key(NULL), NULL)
    expect_equal(redact_key("xyz"), "***")
    expect_equal(redact_key("key12324567890---"), "key123245********")
})



test_that("setupCrunchAuth works", {
    with(temp.option(
        crunch = list(
            crunch.api = "url",
            crunch.api.key = "key",
            crunch.api.test = "test_url",
            crunch.api.key.test = "test_key",
            crunch.api.missingkey = "xxx"
        )), {
            setupCrunchAuth("test")
            expect_identical(
                get_crunch_opt("crunch.api"),
                structure("test_url", source = "setupCrunchAuth('test')")
            )
            expect_identical(
                get_crunch_opt("crunch.api.key"),
                structure("test_key", source = "setupCrunchAuth('test')")
            )
            expect_error(
                setupCrunchAuth("missingboth"),
                "Could not find api in `envOrOption('crunch.api.missingboth')`",
                fixed = TRUE
            )
            expect_error(
                setupCrunchAuth("missingkey"),
                "Could not find key in `envOrOption('crunch.api.key.missingkey')`",
                fixed = TRUE
            )
        }
    )
})
