context("Joining datasets")

join1 <- data.frame(keyvar=c(2, 4, 5, 3), v1=factor(letters[c(2,4)]))
join2 <- data.frame(keyvar=10:1, v2=factor(LETTERS[1:5]))

test_that("Basic join validatation (and warning)", {
    expect_warning(
        expect_error(joinDatasets(join1, join2, copy=FALSE),
            "x must be a Crunch Dataset"),
        "Virtual joins are experimental. Use with extreme caution.")
})

## Alias and wrap joinDatasets to avoid repetition below
join <- function (...) {
    expect_warning(out <- joinDatasets(..., copy=FALSE),
        "Virtual joins are experimental. Use with extreme caution.")
    invisible(out)
}

with_mock_crunch({
    ds1 <- loadDataset("test ds")
    ds2 <- loadDataset("ECON.sav")

    testPayload <- paste0(
        '{"https://app.crunch.io/api/datasets/1/joins/95c0b45fe0af492594863f818cb913d2/":',
        '{"left_key":"https://app.crunch.io/api/datasets/1/variables/birthyr/",',
        '"right_key":"https://app.crunch.io/api/datasets/3/variables/birthyr/"}}')

    test_that("Correct payload without filtering", {
        expect_PATCH(join(ds1, ds2, by.x=ds1$birthyr, ds2$birthyr),
            'https://app.crunch.io/api/datasets/1/joins/',
            testPayload)
    })

    test_that("Can reference variables by alias", {
        expect_PATCH(join(ds1, ds2, by.x="birthyr", by.y="birthyr"),
            'https://app.crunch.io/api/datasets/1/joins/',
            testPayload)
        expect_PATCH(join(ds1, ds2, by="birthyr"),
            'https://app.crunch.io/api/datasets/1/joins/',
            testPayload)
    })

    test_that("Input validation for join", {
        expect_error(join(1),
            "x must be a Crunch Dataset")
        expect_error(join(ds1, 1, by.x=ds1[[1]]),
            "y must be a Crunch Dataset")
        expect_error(join(ds1, ds2, by.x=1),
            "by.x must be a Crunch Variable")
        expect_error(join(ds1, ds2, by.x=ds2[[1]]),
            "by.x must be a variable in x")
        expect_error(join(ds1, ds2, by.x=ds1[[1]], by.y=1),
            "by.y must be a Crunch Variable")
        expect_error(join(ds1, ds2, by.x=ds1[[1]], by.y=ds1[[1]]),
            "by.y must be a variable in y")
        expect_error(join(ds1, ds2, by.x=ds1$birthyr, by.y=ds2$birthyr, all=TRUE),
            'Option "all" not supported.')
        expect_error(join(ds1, ds2, by.x=ds1$birthyr, by.y=ds2$birthyr, all.x=FALSE),
            'Option "all.x=FALSE" not supported.')
        expect_error(join(ds1, ds2, by.x=ds1$birthyr, by.y=ds2$birthyr, all.y=TRUE),
            'Option "all.y" not supported.')
    })
    
    test_that("Categorical and array variables can't be used as keys", {
        expect_error(join(ds1, ds2, by.x=ds1$gender, by.y=ds2$birthyr),
            "by.x must be type numeric or text")
        expect_error(join(ds1, ds2, by.x=ds1$birthyr, by.y=ds2$gender),
            "by.y must be type numeric or text")
    })

    test_that("Providing != 1 alias gives useful error message", {
        expect_error(join(ds1, ds2),
            "by.x must reference one and only one variable")
            ## Default "by" is intersection of names
        expect_error(join(ds1, ds2, by.x=ds1$birthyr),
            "by.y must reference one and only one variable")
    })

    test_that("An invalid alias gives a useful error message", {
        expect_error(join(ds1, ds2, by.x="NOTAVARIABLE"),
            "NOTAVARIABLE does not reference a variable in x")
        expect_error(join(ds1, ds2, by.x=ds1$birthyr, by.y="NOTAVARIABLE"),
            "NOTAVARIABLE does not reference a variable in y")
    })
})

with_test_authentication({
    left <- newDataset(join1)
    right <- newDataset(join2)
    test_that("Join test setup", {
        expect_identical(dim(left), dim(join1))
        expect_identical(dim(right), dim(join2))
        expect_length(joins(left), 0)
    })

    test_that("The join succeeds", {
        joined <- join(left, right, by="keyvar")
        expect_true(is.dataset(joined))
        expect_length(joins(joined), 1)
        skip("TODO: fetch joined variable catalogs")
        expect_identical(dim(joined), c(4L, 3L))
        expect_identical(names(joined), c("keyvar", "v1", "v2"))
        expect_identical(as.vector(joined$v2),
            factor(c("D", "B", "A", "C")))
    })
})
