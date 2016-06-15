context("User stuff")

with_mock_HTTP({
    test_that("Getting user object", {
        expect_is(me(), "UserEntity")
        expect_identical(email(me()), "fake.user@example.com")
    })

    test_that("Getting account's user catalog", {
        usercat <- getAccountUserCatalog()
        expect_is(usercat, "UserCatalog")
        expect_length(usercat, 3)
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
    with_test_authentication({
        test_that("User can be fetched", {
            expect_is(me(), "UserEntity")
        })

        test_that("Create and delete user; cleanup(testUser()) setup/teardown", {
            skip_on_jenkins("Jenkins user needs more permissions")
            u.email <- paste0("test+", as.numeric(Sys.time()), "@crunch.io")
            u.name <- now()
            usercat <- getAccountUserCatalog()
            expect_false(u.email %in% emails(usercat))
            expect_false(u.name %in% sub(" +$", "", names(usercat)))
            with(cleanup(testUser(u.email, u.name)), as="u", {
                usercat <- refresh(usercat)
                expect_true(u.email %in% emails(usercat))
                expect_true(u.name %in% sub(" +$", "", names(usercat)))
                user <- index(usercat)[[self(u)]]
                expect_false(user$account_permissions$create_datasets)
                expect_false(user$account_permissions$alter_users)
            })
            usercat <- refresh(usercat)
            expect_false(u.email %in% emails(usercat))
            expect_false(u.name %in% sub(" +$", "", names(usercat)))
        })

        test_that("User with permissions", {
            skip_on_jenkins("Jenkins user needs more permissions")
            with(cleanup(testUser(advanced=TRUE)), as="u", {
                user <- index(getAccountUserCatalog())[[self(u)]]
                expect_true(user$account_permissions$create_datasets)
                expect_false(user$account_permissions$alter_users)
            })
            with(cleanup(testUser(admin=TRUE)), as="u", {
                user <- index(getAccountUserCatalog())[[self(u)]]
                expect_false(user$account_permissions$create_datasets)
                expect_true(user$account_permissions$alter_users)
            })
            with(cleanup(testUser(admin=TRUE, advanced=TRUE)), as="u", {
                user <- index(getAccountUserCatalog())[[self(u)]]
                expect_true(user$account_permissions$create_datasets)
                expect_true(user$account_permissions$alter_users)
            })
        })
    })

    test_that("User cannot be fetched if logged out", {
        logout()
        expect_error(me(),
            "You must authenticate before making this request")
    })
}
