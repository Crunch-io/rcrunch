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


# Helper to set temporary
with_mock_auth_settings <- function(
  ...,
  env_r_crunch_email = NULL,
  env_r_crunch_pw = NULL,
  opt_crunch_email = NULL,
  opt_crunch_pw = NULL,
  keyring_users = NULL,
  keyring_pws = NULL
) {
  # withr::with_envvar doesn't like NULLs in list
  env_vars <- list()
  env_vars[["R_CRUNCH_EMAIL"]] <- env_r_crunch_email
  env_vars[["R_CRUNCH_PW"]] <- env_r_crunch_pw

  with(
    temp.option(
      crunch = list(
        crunch.email = opt_crunch_email,
        crunch.pw = opt_crunch_pw
      )
    ),
    withr::with_envvar(
      env_vars,
      with_mock(
        `keyring::key_list` = function(service = NULL, keyring = NULL) {
          if (is.null(keyring_users) & is.null(keyring_pws)) {
            data.frame(service = character(0), username = character(0), stringsAsFactors = FALSE)
          } else if(is.null(keyring_users)) {
            data.frame(service = service, username = "", stringsAsFactors = FALSE)
          } else {
            data.frame(service = service, username = keyring_users, stringsAsFactors = FALSE)
          }

        },
        `keyring::key_get` = function(service, username = NULL, keyring = NULL) {
          if (is.null(username)) {
            return(keyring_pws[1])
          } else if (username %in% keyring_users) {
            keyring_pws[username == keyring_users]
          } else {
            stop("Could not find password for email")
          }
        },
        ...
      )
    )
  )
}


test_that("Match username and password with no saved password", {
  #Username passed via text
  with_mock_auth_settings(
    expect_identical(
      get_user_pass_combo(email = "test@crunch.io", password = NULL),
      list(email = "test@crunch.io", password = NULL)
    )
  )

  #Username saved via system environmeent
  with_mock_auth_settings(
    env_r_crunch_email = "test@crunch.io",
    expect_identical(
      get_user_pass_combo(email = NULL, password = NULL),
      list(email = "test@crunch.io", password = NULL)
    )
  )
})

test_that("Match username and password, with single saved keyring password", {

  #Single Keyring password, username specified via login, saved password matches specified username
  with_mock_auth_settings(
    opt_crunch_email = "test@crunch.io",
    opt_crunch_pw = "abc123",
    expect_identical(
      get_user_pass_combo(email = "test@crunch.io", password = NULL),
      list(email = "test@crunch.io", password = "abc123")
    )
  )

  #Single keyring password, username specified via login, saved password for different username
  with_mock_auth_settings(
    keyring_users = "test@crunch.io",
    keyring_pws = "abc123",
    expect_identical(
      suppressWarnings(
        get_user_pass_combo(email = "test@yougov.com", password = NULL)
      ),
      list(email = "test@yougov.com", password = NULL)
    )
  )

  #Single keyring password, username saved in keyring
  with_mock_auth_settings(
    keyring_users = "test@crunch.io",
    keyring_pws = "abc123",
    expect_identical(
      get_user_pass_combo(email = NULL, password = NULL),
      list(email = "test@crunch.io", password = "abc123")
    )
  )

  #Single keyring password, username saved in system environment, saved password matches
  # specified email
  with_mock_auth_settings(
    keyring_users = "test@crunch.io",
    keyring_pws = "abc123",
    env_r_crunch_email = "test@crunch.io",
    expect_identical(
      get_user_pass_combo(email = NULL, password = NULL),
      list(email = "test@crunch.io", password = "abc123")
    )
  )

  #Single keyring password, username saved in system environment, saved password doesn't match
  # specified email
  with_mock_auth_settings(
    keyring_users = "test@crunch.io",
    keyring_pws = "abc123",
    env_r_crunch_email = "test@yougov.com",
    expect_identical(
      suppressWarnings(
        get_user_pass_combo(email = NULL, password = NULL)
      ),
      list(email = "test@yougov.com", password = NULL)
    )
  )

  #Single keyring password, no saved username
  with_mock_auth_settings(
    keyring_users = NULL,
    keyring_pws = "abc123",
    expect_identical(
      get_user_pass_combo(email = NULL, password = NULL),
      list(email = NULL, password = "abc123")
    )
  )

})


test_that("Match username and password, with multiple saved keyring passwords", {
  ## ==== Multiple saved passwords ====

  #Multiple keyring passwords, logging in with username corresponding to saved password
  with_mock_auth_settings(
    keyring_users = c("user@gmail.com", "test@crunch.io"),
    keyring_pws = c("foobar", "abc123"),
    expect_identical(
      get_user_pass_combo(email = "user@gmail.com", password = NULL),
      list(email = "user@gmail.com", password = "foobar")
    )
  )
  with_mock_auth_settings(
    keyring_users = c("user@gmail.com", "test@crunch.io"),
    keyring_pws = c("foobar", "abc123"),
    expect_identical(
      get_user_pass_combo(email = "test@crunch.io", password = NULL),
      list(email = "test@crunch.io", password = "abc123")
    )
  )

  #Multiple keyring passwords, logging in with username NOT corresponding to saved password
  with_mock_auth_settings(
    keyring_users = c("user@gmail.com", "test@crunch.io"),
    keyring_pws = c("foobar", "abc123"),
    expect_identical(
      suppressWarnings(
        get_user_pass_combo(email = "test@yougov.com", password = NULL)
      ),
      list(email = "test@yougov.com", password = NULL)
    )
  )

  # Multiple keyring passwords, attempting to log in without specified username
  with_mock_auth_settings(
    keyring_users = c("user@gmail.com", "test@crunch.io"),
    keyring_pws = c("foobar", "abc123"),
    expect_error(
      get_user_pass_combo(email = NULL, password = NULL),
      "More than one saved Crunch username/email address found in keyring. Try specifying login(email = ...)", # nolint
      fixed = TRUE
    )
  )

  # Username set via environment variable, 2+ saved passwords in keyring, usernane via environemnt
  # variable matching keyring
  with_mock_auth_settings(
    keyring_users = c("user@gmail.com", "test@crunch.io"),
    keyring_pws = c("foobar", "abc123"),
    env_r_crunch_email = "test@crunch.io",
    expect_identical(
      get_user_pass_combo(email = NULL, password = NULL),
      list(email = "test@crunch.io", password = "abc123")
    )
  )

  # Username set via environment variable, 2+ saved passwords in keyring, username via environemnt
  # variable NOT matching keyring
  with_mock_auth_settings(
    keyring_users = c("user@gmail.com", "test@crunch.io"),
    keyring_pws = c("foobar", "abc123"),
    env_r_crunch_email = "test@yougov.com",
    expect_identical(
      suppressWarnings(
        get_user_pass_combo(email = NULL, password = NULL)
      ),
      list(email = "test@yougov.com", password = NULL)
    )
  )
})


test_that("Match username and password, with password saved in RProfile or global environmnent and username in keyring", { # nolint

  #Mutliple usernames saved in keyring
  with_mock_auth_settings(
    keyring_users = c("user@gmail.com", "test@crunch.io"),
    keyring_pws = c("foobar", "abc123"),
    env_r_crunch_pw = "abc123",
    expect_error(
      get_user_pass_combo(email = NULL, password = NULL),
      "More than one saved Crunch username/email address found in keyring. Try specifying login(email = ...)", # nolint
      fixed = TRUE
    )
  )

  #Single username saved in keyring
  with_mock_auth_settings(
    keyring_users = "test@crunch.io",
    keyring_pws = "abc123",
    env_r_crunch_pw = "abc123",
    expect_identical(
      get_user_pass_combo(email = NULL, password = NULL),
      list(email = "test@crunch.io", password = "abc123")
    )
  )
})


test_that(
  "Match username and password, with password and username saved in RProfile or global environemnt", { #nolint

  #Username and password saved
  with_mock_auth_settings(
    env_r_crunch_email = "test@crunch.io",
    env_r_crunch_pw = "abc123",
    expect_identical(
      get_user_pass_combo(email = NULL, password = NULL),
      list(email = "test@crunch.io", password = "abc123")
    )
  )

  # Username only saved
  with_mock_auth_settings(
    env_r_crunch_email = "test@crunch.io",
    expect_identical(
      get_user_pass_combo(email = NULL, password = NULL),
      list(email = "test@crunch.io", password = NULL)
    )
  )
  with_mock_auth_settings(
    env_r_crunch_email = "test@crunch.io",
    expect_identical(
      get_user_pass_combo(email = "test@yougov.com", password = NULL),
      list(email = "test@yougov.com", password = NULL)
    )
  )

  # Password only saved
  with_mock_auth_settings(
    env_r_crunch_pw = "abc123",
    expect_identical(
      get_user_pass_combo(email = NULL, password = NULL),
      list(email = NULL, password = "abc123")
    )
  )
  # Will generate warning if interactive
  with_mock_auth_settings(
    env_r_crunch_pw = "abc123",
    expect_identical(
      get_user_pass_combo(email = "test@crunch.io", password = NULL),
      list(email = "test@crunch.io", password = "abc123")
    )
  )
})


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


if (run.integration.tests) {
    with(test_options, {
        test_that("crunchAuth succeeds when it should and not when it shouldn't", {
            suppressWarnings(logout())
            expect_error(
                suppressWarnings(crunchAuth("lkjasdfksdfkjhl", password = "w23nrnsod")),
                "Unable to authenticate lkjasdfksdfkjhl"
            )
        })

        test_that("login returns a session object", {
            expect_warning(cr <- suppressMessages(login()), "deprecated")
            expect_true(is.list(cr))
            expect_warning(logout(), "deprecated")
        })
    })
}
