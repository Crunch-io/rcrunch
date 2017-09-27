context("Various helper functions")

test_that("is.error", {
    e <- try(halt("error in a box"), silent=TRUE)
    expect_true(is.error(e))
    expect_false(is.error("not an error"))
    expect_false(is.error(NULL))
    expect_false(is.error(NA))
    expect_error("not an error", NA)
})

test_that("rethrow a caught error", {
    e <- try(halt("error in a box"), silent=TRUE)
    expect_true(is.error(e))
    expect_error(rethrow(e), "error in a box")
})

test_that("%||%", {
    expect_identical("f" %||% "g", "f")
    expect_identical(NULL %||% "g", "g")
    expect_identical("f" %||% halt("Nooooooo!"), "f")
})

test_that("dirtyElements", {
    x <- list(
        list(a=1, b=1),
        list(a="1", b="1"),
        list(a="d", b="e")
    )
    y <- x
    expect_false(any(dirtyElements(x, y)))
    y[[2]]$b <- "f"
    y[[1]]$b <- 1
    expect_identical(dirtyElements(x, y), c(FALSE, TRUE, FALSE))
    y[[3]]$a <- "f"
    expect_identical(dirtyElements(x, y), c(FALSE, TRUE, TRUE))
})


test_that("joinPath", {
    expect_identical(joinPath("https://app.crunch.io/api/datasets/", "../variables/"),
        "https://app.crunch.io/api/variables/")
    expect_identical(joinPath("https://app.crunch.io/api/variables/", "4412es/"),
        "https://app.crunch.io/api/variables/4412es/")
    expect_identical(joinPath("a/b/c/d/../e/f/", "g/../../h/"),
        "a/b/c/e/h/")
    expect_identical(joinPath("https://app.crunch.io/api/datasets/", "/variables/"),
        "/variables/")
    expect_identical(joinPath("https://app.crunch.io/api/datasets/", "/"),
        "/")
    expect_identical(joinPath("https://app.crunch.io/api/datasets/", "./id/"),
        "https://app.crunch.io/api/datasets/id/")
})

test_that("absoluteURL", {
    base.url <- "https://fake.crunch.io/api/datasets/"
    expect_identical(absoluteURL("../variables/", base.url),
        "https://fake.crunch.io/api/variables/")
    expect_identical(absoluteURL("4412es/", base.url),
        "https://fake.crunch.io/api/datasets/4412es/")
    expect_identical(absoluteURL("g/../../h/",
        "https://fake.crunch.io/a/b/c/d/../e/f/"),
        "https://fake.crunch.io/a/b/c/e/h/")
    expect_identical(absoluteURL("/variables/", base.url),
        "https://fake.crunch.io/variables/")
    expect_identical(absoluteURL("/", base.url),
        "https://fake.crunch.io/")
})

test_that("emptyObject JSONifies correctly", {
    expect_equal(unclass(toJSON(emptyObject())), "{}")
    expect_equal(unclass(toJSON(emptyObject(list(a=1), 1:4))), "{}")
})

test_that("null function always returns null", {
    expect_null(null())
    expect_null(null(TRUE))
    expect_null(null(stop("yo!")))
})

test_that("JSON behavior for NULL (handle jsonlite API change in 0.9.22)", {
    expect_equal(unclass(toJSON(NULL)), "{}")
    expect_equal(unclass(toJSON(list(x=NULL))), '{"x":null}')
})

test_that("setIfNotAlready", {
    with(temp.options(crunch.test.opt1="previous",
                      crunch.test.opt2=NULL,
                      crunch.test.opt3=4), {

        old <- setIfNotAlready(crunch.test.opt1="value", crunch.test.opt2=5)
        expect_identical(getOption("crunch.test.opt1"), "previous")
        expect_identical(getOption("crunch.test.opt2"), 5)
        expect_identical(getOption("crunch.test.opt3"), 4)
    })
})

test_that("startsWith/endsWith for old R", {
    expect_true(alt.startsWith("http://", "http"))
    expect_false(alt.startsWith("http://", "https"))
    expect_true(alt.endsWith("http://", "//"))
    expect_false(alt.endsWith("http://", "http"))
})

test_that("uniquify", {
    expect_identical(uniquify(rep("a", 4)),
        c("a", "a  (1)", "a  (2)", "a  (3)"))
    expect_identical(uniquify(c("b", "a", "a", "abcd", "a")),
        c("b", "a", "a  (1)", "abcd", "a  (2)"))
})

test_that("vectorOrList", {
    expect_true(vectorOrList(c("a", "b", "c"), "character"))
    expect_true(vectorOrList(list("a", "b", "c"), "character"))
    expect_false(vectorOrList(c(1, 2, 3), "character"))
    expect_false(vectorOrList(list("a", 1, "c"), "character"))
    expect_true(vectorOrList(c(1, 2, 3), "numeric"))
    expect_false(vectorOrList(list("a", 1, "c"), "numeric"))
})

test_that("setCrunchAPI", {
    old_crunch_api <- getOption("crunch.api")
    setCrunchAPI('foobar')
    expect_equal(getOption("crunch.api"), "https://foobar.crunch.io/api/")
    setCrunchAPI('barfoo', 8888)
    expect_equal(getOption("crunch.api"), "http://barfoo.crunch.io:8888/api/")
    options("crunch.api" = old_crunch_api)
})
