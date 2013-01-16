context("Authentication")

test_that("On package load, the session_store exists", {
    expect_true(is.environment(session_store))
})

test_that("setToken saves a cookie in the session_store", {
    saveToken(list(token="fake.user@crunch.test"))
    expect_true(exists("cookie", envir=session_store))
    expect_true(is.list(session_store$cookie))
})

test_that("getToken can retrieve a token", {
    test.token <- getToken()
    expect_identical(class(test.token), "config")
})

test_that("Logging out deletes cookies", {
    expect_true("cookie" %in% ls(envir=session_store))
    logout()
    expect_false("cookie" %in% ls(envir=session_store))
})

test_that("basicAuthArgs", {
    expect_true(is.character(basicAuthArgs("myemail")))
    expect_true(is.list(RJSONIO:::fromJSON(basicAuthArgs("myemail"),
        simplifyWithNames=FALSE)))
})

test_that("login works if crunch is running", {
    logout()
    login("***REMOVED***")
    expect_identical(class(getToken()), "config")
})