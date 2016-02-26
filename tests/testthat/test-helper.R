context("Things in helper.R")

with(temp.option(foo.bar="no", foo.other="other"), {
    ## Mock outside of test_that because the expectations also check Sys.getenv
    with_mock(`Sys.getenv`=function (q, ...) {
            return(list(R_FOO_BAR="yes")[[q]] %||% "")
        },
        e1 <- envOrOption("foo.bar"),
        e2 <- envOrOption("foo.other"),
        e3 <- envOrOption("somethingelse")
    )

    test_that("envOrOption gets the right thing", {
        expect_identical(e1, "yes") ## Env var trumps option
        expect_identical(envOrOption("foo.bar"), "no") ## Bc no more env var
        expect_identical(e2, "other") ## Option if there is no env var
        expect_identical(e3, NULL) ## Null if neither
    })
})

test_that("SUTD", {
    a <- NULL
    tester <- setup.and.teardown(function () a <<- FALSE,
        function () a <<- TRUE)

    expect_true(is.null(a))
    with(tester, {
        expect_false(is.null(a))
        expect_false(a)
        ## Test that assertion failures are raised
        # expect_false(TRUE)
    })
    expect_true(a)

    ## Test that even if the code in the with block throws an error, (1) the
    ## teardown is run, and (2) it doesn't fail silently but turns into a
    ## failed test expectation.
    # a <- NULL
    # expect_true(is.null(a))
    # with(tester, {
    #     expect_false(is.null(a))
    #     expect_false(a)
    #     halt("Testing error handling, please ignore")
    # })
    # expect_true(a)
})
