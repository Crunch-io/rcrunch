context("Authentication")

test_that("On package load, the auth_store exists", {
    # expect_true(is.environment(rcrunch:::auth_store))
    expect_true(is.environment(auth_store))
})

test_that("Authentication sets a cookie in the auth_store", {
    login(email="fake.user@crunch.test")
    expect_true(exists("cookie", envir=auth_store))
    expect_true(is.list(auth_store$cookie))
})

test_that("Logging out deletes cookies", {
    auth_store$cookie <- "something"
    expect_false(length(ls(envir=auth_store))==0)
    logout()
    expect_true(length(ls(envir=auth_store))==0)
})