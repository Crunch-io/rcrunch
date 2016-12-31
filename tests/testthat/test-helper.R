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
        expect_null(e3) ## Null if neither
    })
})
