context("Authentication")

test_that("On package load, the auth_store exists", {
    expect_true(is.environment(auth_store))
})

test_that("setToken saves a cookie in the auth_store", {
    saveToken(list(token="fake.user@crunch.test"))
    expect_true(exists("cookie", envir=auth_store))
    expect_true(is.list(auth_store$cookie))
})

test_that("getToken can retrieve a token", {
    test.token <- getToken()
    expect_identical(class(test.token), "config")
})

test_that("Logging out deletes cookies", {
    expect_false(length(ls(envir=auth_store))==0)
    logout()
    expect_true(length(ls(envir=auth_store))==0)
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