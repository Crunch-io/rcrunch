context("User stuff")

with_mock_crunch({
    test_that("Getting user object", {
        expect_is(me(), "UserEntity")
        expect_identical(email(me()), "fake.user@example.com")
    })

    test_that("Getting account's user catalog", {
        usercat <- getAccountUserCatalog()
        expect_is(usercat, "UserCatalog")
        expect_length(usercat, 3)
        expect_identical(urls(usercat),
            c("https://app.crunch.io/api/users/user1/",
              "https://app.crunch.io/api/users/user3/",
              "https://app.crunch.io/api/users/user2/"))
        expect_identical(names(usercat),
            c("Fake User", "Bill User", "Roger User"))
        expect_identical(emails(usercat),
            c("fake.user@example.com",
              "william.user@example.io",
              "ruser@crunch.io"))
    })

    test_that("Reset password", {
        expect_POST(resetPassword("me@example.com"),
            "https://app.crunch.io/api/public/password_reset/",
            '{"email":"me@example.com",',
            '"url_base":"https://app.crunch.io/password/change/${token}/"}')
    })

    test_that("invite (not currently exported)", {
        expect_POST(invite("me@example.com", name="Me"),
            "https://app.crunch.io/api/accounts/account1/users/",
            '{"element":"shoji:entity","body":{"email":"me@example.com",',
            '"send_invite":true,"id_method":"pwhash",',
            '"account_permissions":{"alter_users":false,',
            '"create_datasets":false},"first_name":"Me",',
            '"url_base":"/password/change/${token}/"}}')
    })
})

with_test_authentication({
    test_that("User can be fetched", {
        expect_is(me(), "UserEntity")
    })

    test_that("Create and delete user", {
        skip_on_jenkins("Jenkins user needs more permissions")
        u.email <- paste0("test+", as.numeric(Sys.time()), "@crunch.io")
        u.name <- now()
        usercat <- getAccountUserCatalog()
        expect_false(u.email %in% emails(usercat))
        expect_false(u.name %in% sub(" +$", "", names(usercat)))
        u <- testUser(u.email, u.name)
        usercat <- refresh(usercat)
        expect_true(u.email %in% emails(usercat))
        expect_true(u.name %in% sub(" +$", "", names(usercat)))
        user <- index(usercat)[[self(u)]]
        expect_false(user$account_permissions$create_datasets)
        expect_false(user$account_permissions$alter_users)
        crDELETE(self(u))
        usercat <- refresh(usercat)
        expect_false(u.email %in% emails(usercat))
        expect_false(u.name %in% sub(" +$", "", names(usercat)))
    })

    test_that("User with permissions", {
        skip_on_jenkins("Jenkins user needs more permissions")
        u1 <- testUser(advanced=TRUE)
        user <- index(getAccountUserCatalog())[[self(u1)]]
        expect_true(user$account_permissions$create_datasets)
        expect_false(user$account_permissions$alter_users)
        u2 <- testUser(admin=TRUE)
        user <- index(getAccountUserCatalog())[[self(u2)]]
        expect_false(user$account_permissions$create_datasets)
        expect_true(user$account_permissions$alter_users)
        u3 <- testUser(admin=TRUE, advanced=TRUE)
        user <- index(getAccountUserCatalog())[[self(u3)]]
        expect_true(user$account_permissions$create_datasets)
        expect_true(user$account_permissions$alter_users)
    })
})

test_that("User cannot be fetched if logged out", {
    logout()
    expect_error(me(),
        "You must authenticate before making this request")
})
