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

test_that("session info can be deleted out deletes cookies", {
    expect_true("cookie" %in% ls(envir=session_store))
    deleteSessionInfo()
    expect_false("cookie" %in% ls(envir=session_store))
})

test_that("basicAuthArgs", {
    expect_true(is.character(basicAuthArgs("myemail")))
    expect_true(is.list(RJSONIO:::fromJSON(basicAuthArgs("myemail"),
        simplifyWithNames=FALSE)))
})

if (!run.only.local.tests) {
    test_that("login works if crunch is running", {
        deleteSessionInfo()
        login(test.user)
            expect_identical(class(getToken()), "config")
            expect_true("urls" %in% ls(envir=session_store))
            expect_true(is.authenticated())
        logout()
    })

    test_that("crunchAuth succeeds when it should and not when it shouldn't", {
        logout()
        expect_true(is.list(crunchAuth(test.user)))
        login(test.user) ## so we can logout; crunch API is misbehaving
        logout()
        expect_error(crunchAuth("lkjasdfksdfkjhl"), 
            "Unable to authenticate lkjasdfksdfkjhl")
    })

    test_that("session URLs can be retrieved", {
        login(test.user)
            expect_true(is.character(sessionURL("user_url")))
            expect_true(is.list(sessionURL()))
        logout()
        expect_error(sessionURL("user_url"), 
            "You must authenticate before making this request")
    })
}
