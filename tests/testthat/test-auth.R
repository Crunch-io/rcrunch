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


# Save actual passwords, before testing with fake passwords
keyring_saved_usernames <- keyring::key_list("crunch")$username
keyring_saved_passwords <- sapply(keyring_saved_usernames, function(x) keyring::key_get("crunch", username = x))
env_saved_username <- Sys.getenv("R_CRUNCH_EMAIL")
env_saved_pw <- Sys.getenv("R_CRUNCH_PW")
rprof_saved_username <- getOption("crunch.email")
rprof_saved_pw <- getOption("crunch.pw")

#Delete real saved passwords
if(nrow(keyring::key_list("crunch")) != 0) keyring::key_delete("crunch")
Sys.unsetenv("R_CRUNCH_EMAIL")
Sys.unsetenv("R_CRUNCH_PW")
options(crunch.email = NULL)
options(crunch.pw = NULL)



test_that("Match username and password with no saved password", {
  
  #Username passed via text
  expect_identical(
    get_user_pass_combo(email = "test@crunch.io", password = NULL),
    list(email = "test@crunch.io", password = NULL)
  )
  
  #Username saved via system environmeent
  Sys.setenv(R_CRUNCH_EMAIL = "test@crunch.io")
  expect_identical(
    get_user_pass_combo(email = NULL, password = NULL),
    list(email = "test@crunch.io", password = NULL)
  )
  
  Sys.unsetenv("R_CRUNCH_EMAIL")
})

test_that("Match username and password, with single saved keyring password", {
  
  #Single Keyring password, username specified via login, saved password matches specified username
  keyring::key_set_with_value("crunch", username = "test@crunch.io", password = "abc123")
  expect_identical(
    get_user_pass_combo(email = "test@crunch.io", password = NULL),
    list(email = "test@crunch.io", password = "abc123")
  )
  
  #Single keyring password, username specified via login, saved password for different username
  expect_identical(
    get_user_pass_combo(email = "test@yougov.com", password = NULL),
    list(email = "test@yougov.com", password = NULL)
  )
  expect_warning(
    get_user_pass_combo(email = "test@yougov.com", password = NULL),
    "Saved Crunch passwords in keyring do not match specified email"
  )
  
  #Single keyring password, username saved in keyring
  expect_identical(
    get_user_pass_combo(email = NULL, password = NULL),
    list(email = "test@crunch.io", password = "abc123")
  )
  
  #Single keyring password, username saved in system environment, saved password matches specified email
  #expected outcome - test@crunch.io, abc123
  Sys.setenv(R_CRUNCH_EMAIL = "test@crunch.io")
  expect_identical(
    get_user_pass_combo(email = NULL, password = NULL),
    list(email = "test@crunch.io", password = "abc123")
  )
  Sys.unsetenv("R_CRUNCH_EMAIL")
  
  #Single keyring password, username saved in system environment, saved password doesn't match specified email
  #Expected outcome - test@yougov.com, NULL, warning "Saved Crunch passwords in keyring do not match specified email"
  Sys.setenv(R_CRUNCH_EMAIL = "test@yougov.com")
  expect_identical(
    get_user_pass_combo(email = NULL, password = NULL),
    list(email = "test@yougov.com", password = NULL)
  )
  expect_warning(
    get_user_pass_combo(email = NULL, password = NULL),
    "Saved Crunch passwords in keyring do not match specified email"
  )
  Sys.unsetenv("R_CRUNCH_EMAIL")
  
  #Single keyring password, no saved username
  keyring::key_delete("crunch")
  keyring::key_set_with_value("crunch", password = "abc123")
  expect_identical(
    get_user_pass_combo(email = NULL, password = NULL),
    list(email = NULL, password = "abc123")
  )
  
  #Clean up
  keyring::key_delete("crunch")
})


test_that("Match username and password, with multiple saved keyring passwords", {
  ## ==== Multiple saved passwords ====
  
  #Multiple keyring passwords, logging in with username corresponding to saved password
  keyring::key_set_with_value("crunch", username = "user@gmail.com", password = "foobar")
  keyring::key_set_with_value("crunch", username = "test@crunch.io", password = "abc123")
  expect_identical(
    get_user_pass_combo(email = "user@gmail.com", password = NULL),
    list(email = "user@gmail.com", password = "foobar")
  )
  expect_identical(
    get_user_pass_combo(email = "test@crunch.io", password = NULL),
    list(email = "test@crunch.io", password = "abc123")
  )
  
  #Multiple keyring passwords, logging in with username NOT corresponding to saved password
  expect_identical(
    get_user_pass_combo(email = "test@yougov.com", password = NULL),
    list(email = "test@yougov.com", password = NULL)
  )
  expect_warning(
    get_user_pass_combo(email = "test@yougov.com", password = NULL),
    "Saved Crunch passwords in keyring do not match specified email"
  )
  
  #Multiple keyring passwords, attempting to log in without specified username
  expect_error(
    get_user_pass_combo(email = NULL, password = NULL),
    "More than one saved Crunch username/email address found in keyring. Try specifying login(email = ...)",
    fixed = TRUE
  )
  
  #Username set via environment variable, 2+ saved passwords in keyring, usernane via environemnt variable matching keyring
  Sys.setenv(R_CRUNCH_EMAIL = "test@crunch.io")
  expect_identical(
    get_user_pass_combo(email = NULL, password = NULL),
    list(email = "test@crunch.io", password = "abc123")
  )
  Sys.unsetenv("R_CRUNCH_EMAIL")
  
  #Username set via environment variable, 2+ saved passwords in keyring, username via environemnt variable NOT matching keyring
  #Expected outcome - test@yougov.com, NULL
  Sys.setenv(R_CRUNCH_EMAIL = "test@yougov.com")
  expect_identical(
    get_user_pass_combo(email = NULL, password = NULL),
    list(email = "test@yougov.com", password = NULL)
  )
  expect_warning(
    get_user_pass_combo(email = NULL, password = NULL),
    "Saved Crunch passwords in keyring do not match specified email"
  )
  Sys.unsetenv("R_CRUNCH_EMAIL")
  
  #Clean up
  keyring::key_delete("crunch", username = "user@gmail.com")
  keyring::key_delete("crunch", username = "test@crunch.io")
  
})

test_that("Match username and password, with password saved in RProfile or global environmnent and username in keyring", {
  
  keyring::key_set_with_value("crunch", username = "user@gmail.com", password = "foobar")
  keyring::key_set_with_value("crunch", username = "test@crunch.io", password = "abc123")
  
  #Mutliple usernames saved in keyring
  Sys.setenv(R_CRUNCH_PW = "abc123")
  expect_error(
    get_user_pass_combo(email = NULL, password = NULL),
    "More than one saved Crunch username/email address found in keyring. Try specifying login(email = ...)",
    fixed = TRUE
  )
  Sys.unsetenv("R_CRUNCH_PW")
  
  #Single username saved in keyring
  Sys.setenv(R_CRUNCH_PW = "abc123")
  keyring::key_delete("crunch", username = "user@gmail.com")
  expect_identical(
    get_user_pass_combo(email = NULL, password = NULL),
    list(email = "test@crunch.io", password = "abc123")
  )
  Sys.unsetenv("R_CRUNCH_PW")
  
  #Clean up
  keyring::key_delete("crunch", username = "test@crunch.io")
})


test_that("Match username and password, with password and username saved in RProfile or global environemnt", {
  
  Sys.setenv(R_CRUNCH_PW = "abc123")
  Sys.setenv(R_CRUNCH_EMAIL = "test@crunch.io")
  
  expect_identical(
    get_user_pass_combo(email = NULL, password = NULL),
    list(email = "test@crunch.io", password = "abc123")
  )
  
  # Username only saved
  Sys.unsetenv("R_CRUNCH_PW")
  expect_identical(
    get_user_pass_combo(email = NULL, password = NULL),
    list(email = "test@crunch.io", password = NULL)
  )
  expect_identical(
    get_user_pass_combo(email = "test@yougov.com", password = NULL),
    list(email = "test@yougov.com", password = NULL)
  )
  
  # Password only saved
  Sys.unsetenv("R_CRUNCH_EMAIL")
  Sys.setenv(R_CRUNCH_PW = "abc123")
  #Expected value - NULL, abc123
  expect_identical(
    get_user_pass_combo(email = NULL, password = NULL),
    list(email = NULL, password = "abc123")
  )
  
  #Expected value: test@crunch.io, abc123, warning if interactive
  expect_identical(
    get_user_pass_combo(email = "test@crunch.io", password = NULL),
    list(email = "test@crunch.io", password = "abc123")
  )
  
  #Clean up
  Sys.unsetenv("R_CRUNCH_PW")
})

# Restore real saved passwords
if(length(keyring_saved_usernames) >= 1){
  for(i in 1:length(keyring_saved_usernames)){
    keyring::key_set_with_value("crunch", username = keyring_saved_usernames[i], password = keyring_saved_passwords[i])
  }
}
Sys.setenv(R_CRUNCH_EMAIL = env_saved_username)
Sys.setenv(R_CRUNCH_PW = env_saved_pw)
options(crunch.email = rprof_saved_username)
options(crunch.pw = rprof_saved_pw)
rm(keyring_saved_usernames, keyring_saved_passwords, env_saved_username, env_saved_pw, rprof_saved_username, rprof_saved_pw)


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
            expect_identical(cfg$options$cookie, "token=test_token")
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
