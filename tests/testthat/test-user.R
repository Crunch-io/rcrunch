context("User stuff")

with(fake.HTTP, {
    test_that("Getting user object", {
        user <- getUser("/api/users/user1.json")
        expect_true(inherits(user, "ShojiObject"))
        expect_identical(user@body$email, "fake.user@example.com")
    })
    
    test_that("Getting account's user catalog", {
        usercat <- getAccountUserCatalog()
        expect_true(inherits(usercat, "UserCatalog"))
        expect_identical(length(usercat), 3L)
        expect_identical(urls(usercat), 
            c("/api/users/user1.json", 
              "/api/users/user3.json", 
              "/api/users/user2.json"))
        expect_identical(names(usercat), 
            c("Fake User", "Bill User", "Roger User"))
        expect_identical(emails(usercat), 
            c("fake.user@example.com",
              "william.user@example.io",
              "ruser@crunch.io"))
    })
})

if (run.integration.tests) {
    test_that("User can be fetched", {
        with(test.authentication, {
            user <- try(getUser())
            expect_false(is.error(user))
            expect_true(inherits(user, "ShojiObject"))
        })
    })

    test_that("User cannot be fetched if logged out", {
        logout()
        expect_error(getUser(), 
            "You must authenticate before making this request")
    })
}
