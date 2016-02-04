context("Caching")

with_mock_GET <- function (expr) {
    with_mock(
        `httr::GET`=function (url, ...) list(response=nchar(url), status_code=200),
        eval.parent(expr)
    )
}

clearCache()
with_mock_GET({
    cGET("https://beta.crunch.io/api/datasets")
    cGET("https://beta.crunch.io/api/", query=list(user="me"))
})

test_that("cache gets set on GET", {
    expect_identical(length(ls(envir=cache)), 2L)
    expect_true("https://beta.crunch.io/api/datasets" %in% ls(envir=cache))
})

clearCache()
with(no.cache(), {
    with_mock_GET({
        z <- cGET("https://beta.crunch.io/api/users/", query=list(query=rep("Q", 10000)))
    })
})
test_that("Checking cache even with cache off doesn't fail on long query", {
    expect_true(is.numeric(z$response))
})

clearCache()
with_mock_GET({
    cGET("https://beta.crunch.io/api/users/", query=list(query=rep("Q", 10000)))
})

test_that("cache gets set on GET even with long query", {
    expect_identical(length(ls(envir=cache)), 1L)
    expect_true(any(grepl("https://beta.crunch.io/api/users/",
        ls(envir=cache), fixed=TRUE)))
})
