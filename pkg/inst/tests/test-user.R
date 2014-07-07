context("User stuff")

with(fake.HTTP, {
    test_that("Getting user object", {
        user <- getUser("api/users/user1.json")
        expect_true(inherits(user, "ShojiObject"))
        expect_identical(user$email, "fake.user@example.com")
    })
})

if (!run.only.local.tests) {
    test_that("User can be fetched", {
        with(test.authentication, {
            user <- try(getUser())
            expect_false(is.error(user))
            expect_true(inherits(user, "ShojiObject"))
            expect_true(!is.null(session_store$user))
        })
    })

    test_that("User cannot be fetched if logged out", {
        logout()
        expect_error(getUser(), 
            "You must authenticate before making this request")
    })
}
