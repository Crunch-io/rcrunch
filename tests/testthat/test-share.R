context("Sharing")

## Permissions catalog
## emails

## Share one dataset with many
## Share many datasets
me <- "fake.user@example.com"

with(fake.HTTP, {
    ds <- loadDataset("test ds")
    test_that("Dataset has permissions catalog", {
        expect_true(inherits(permissions(ds), "PermissionCatalog"))
        expect_identical(urls(permissions(ds)), 
            c("/api/users/user1.json", "/api/users/user2.json"))
        expect_identical(emails(permissions(ds)),
            c("fake.user@example.com", "nobody@crunch.io"))
    })
    test_that("Editing attributes", {
        expect_identical(is.editor(permissions(ds)),
            structure(c(TRUE, FALSE), 
            .Names=c("fake.user@example.com", "nobody@crunch.io")))
        expect_true(userCanEdit(me, ds))
        expect_false(userCanEdit("nobody@crunch.io", ds))
        expect_true(userCanView("nobody@crunch.io", ds))
        expect_false(userCanView("not.a.user@hotmail.com", ds))
        skip("TODO things with User entity")
        expect_true(iCanEdit(ds))
    })
})

me <- getOption("crunch.email")

if (run.integration.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            test_that("PermissionsCatalog from real dataset", {
                expect_true(inherits(permissions(ds), "PermissionCatalog"))
                expect_identical(urls(permissions(ds)), 
                    userURL())
                expect_identical(emails(permissions(ds)),
                    me)
                expect_identical(is.editor(permissions(ds)),
                    structure(TRUE, .Names=me))
            })
            
            test_that("share method for dataset", {
                try(share(ds, "foo@crunch.io", notify=FALSE))
                expect_true(setequal(emails(permissions(ds)),
                    c(me, "foo@crunch.io")))
            })
            
            test_that("re-sharing doesn't change the state", {
                try(share(ds, "foo@crunch.io", notify=FALSE))
                expect_true(setequal(emails(permissions(ds)),
                    c(me, "foo@crunch.io")))
            })
            
            others <- c("foo@crunch.io", "a@crunch.io", "b@crunch.io")
            test_that("can share dataset with multiple at same time", {
                try(share(ds, c("a@crunch.io", "b@crunch.io"), notify=FALSE))
                expect_true(setequal(emails(permissions(ds)),
                    c(me, others)))
                expect_true(userCanEdit(me, ds))
                for (user in others) {
                    expect_false(userCanEdit(user, ds), info=user)
                }
            })
            
            test_that("can transfer editor privileges", {
                skip("Need to create full users in order to test passing ball")
                try(share(ds, "foo@crunch.io", notify=FALSE, edit=TRUE))
                expect_true(userCanEdit("foo@crunch.io", ds))
                expect_false(userCanEdit(me, ds))
            })
            
            test_that("Cannot remove only editor", {
                skip("Need to create full users in order to test passing ball")
                expect_error(share(ds, "foo@crunch.io", notify=FALSE, edit=FALSE),
                    "Cannot remove editor from the dataset without specifying another")
            })
            
            test_that("Cannot unmake myself editor without passing", {
                try(share(ds, me, notify=FALSE, edit=TRUE))
                expect_true(userCanEdit(me, ds))
                expect_error(share(ds, me, notify=FALSE, edit=FALSE),
                    "Cannot remove editor from the dataset without specifying another")
            })
            
            test_that("Cannot make multiple people editors", {
                expect_error(share(ds, c("a@crunch.io", "b@crunch.io"),
                    notify=FALSE, edit=TRUE), 
                    "Can only set one user as editor")
            })
        })
    })
}