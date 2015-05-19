context("Sharing")

## Permissions catalog
## emails

## Share one dataset with many
## Share many datasets

me <- getOption("crunch.email")

with(fake.HTTP, {
    ds <- loadDataset("test ds")
    test_that("Dataset has permissions catalog", {
        expect_true(inherits(permissions(ds), "PermissionsCatalog"))
        expect_identical(urls(permissions(ds)), 
            )
        expect_identical(emails(permissions(ds)),
            )
    })
    test_that("Editing attributes", {
        expect_identical(is.editor(permission(ds)),
            )
        expect_true(userCanEdit("", ds))
        expect_false(userCanEdit("", ds))
        expect_true(iCanEdit(ds))
    })
})

if (run.integration.tests) {
    with(test.authentication, {
        with(test.dataset(ds), {
            test_that("PermissionsCatalog from real dataset", {
                expect_true(inherits(permissions(ds), "PermissionsCatalog"))
                expect_identical(urls(permissions(ds)), 
                    )
                expect_identical(emails(permissions(ds)),
                    )
                expect_identical(is.editor(permission(ds)),
                    )
            })
            
            test_that("share method for dataset", {
                try(share(ds, "foo@crunch.io", send=FALSE))
                
            })
            
            test_that("re-sharing doesn't change the state", {
                ## Copy above expectations again
            })
            
            test_that("can share dataset with multiple at same time", {
                try(share(ds, c("a@crunch.io", "b@crunch.io"), send=FALSE))
                
            })
            
            test_that("can transfer editor privileges", {
                try(share(ds, "foo@crunch.io", send=FALSE, edit=TRUE))
                expect_true(userCanEdit("foo@crunch.io", ds))
                expect_false(iCanEdit(ds))
            })
            
            test_that("Cannot remove only editor", {
                expect_error(share(ds, "foo@crunch.io", send=FALSE, edit=FALSE),
                    "Correct error message")
            })
            
            test_that("Cannot unmake myself editor without passing", {
                try(share(ds, me, send=FALSE, edit=TRUE))
                expect_true(iCanEdit(ds))
                expec_error(share(ds, me, send=FALSE, edit=FALSE),
                    "Correct error message")
            })
            
            test_that("Cannot make multiple people editors", {
                expect_error(share(ds, c("a@crunch.io", "b@crunch.io"),
                    send=FALSE, edit=TRUE), 
                    "Correct error message")
            })
        })
    })
}