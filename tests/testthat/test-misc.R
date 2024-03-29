context("Various helper functions")

test_that("can warn and message once", {
    expect_warning(warn_once("danger!", option = "test_warn_once"), "danger!")
    expect_warning(warn_once("danger!", option = "test_warn_once"), NA)
    expect_message(message_once("hi!", option = "test_msg_once"), "hi!")
    expect_message(message_once("hi!", option = "test_msg_once"), NA)
})


test_that("is.error", {
    e <- try(halt("error in a box"), silent = TRUE)
    expect_true(is.error(e))
    expect_false(is.error("not an error"))
    expect_false(is.error(NULL))
    expect_false(is.error(NA))
    expect_error("not an error", NA)
})

test_that("rethrow a caught error", {
    e <- try(halt("error in a box"), silent = TRUE)
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
        list(a = 1, b = 1),
        list(a = "1", b = "1"),
        list(a = "d", b = "e")
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
    expect_identical(
        joinPath("https://app.crunch.io/api/datasets/", "../variables/"),
        "https://app.crunch.io/api/variables/"
    )
    expect_identical(
        joinPath("https://app.crunch.io/api/variables/", "4412es/"),
        "https://app.crunch.io/api/variables/4412es/"
    )
    expect_identical(
        joinPath("a/b/c/d/../e/f/", "g/../../h/"),
        "a/b/c/e/h/"
    )
    expect_identical(
        joinPath("https://app.crunch.io/api/datasets/", "/variables/"), # nolint
        "/variables/" # nolint
    )
    expect_identical(
        joinPath("https://app.crunch.io/api/datasets/", "/"),
        "/"
    )
    expect_identical(
        joinPath("https://app.crunch.io/api/datasets/", "./id/"),
        "https://app.crunch.io/api/datasets/id/"
    )
})

test_that("absoluteURL", {
    base.url <- "https://fake.crunch.io/api/datasets/"
    expect_identical(
        absoluteURL("../variables/", base.url),
        "https://fake.crunch.io/api/variables/"
    )
    expect_identical(
        absoluteURL("4412es/", base.url),
        "https://fake.crunch.io/api/datasets/4412es/"
    )
    expect_identical(
        absoluteURL(
            "g/../../h/",
            "https://fake.crunch.io/a/b/c/d/../e/f/"
        ),
        "https://fake.crunch.io/a/b/c/e/h/"
    )
    expect_identical(
        absoluteURL("/variables/", base.url), # nolint
        "https://fake.crunch.io/variables/"
    )
    expect_identical(
        absoluteURL("/", base.url),
        "https://fake.crunch.io/"
    )
})

test_that("emptyObject JSONifies correctly", {
    expect_equal(unclass(toJSON(emptyObject())), "{}")
    expect_equal(unclass(toJSON(emptyObject(list(a = 1), 1:4))), "{}")
})

test_that("null function always returns null", {
    expect_null(null())
    expect_null(null(TRUE))
    expect_null(null(stop("yo!")))
})

test_that("JSON behavior for NULL (handle jsonlite API change in 0.9.22)", {
    expect_equal(unclass(toJSON(NULL)), "{}")
    expect_equal(unclass(toJSON(list(x = NULL))), '{"x":null}')
})

test_that("toJSON sorts when we want", {
    unsorted <- '{"b":1,"a":2}'
    sorted <- '{"a":2,"b":1}'
    expect_equal(
        unclass(toJSON(list(b = 1, a = 2))),
        unsorted
    )
    expect_equal(
        unclass(toJSON(list(b = 1, a = 2), for_query_string = TRUE)),
        unsorted
    )
    expect_equal(
        unclass(with(
            temp.options(crunch = list(crunch.stabilize.query = TRUE)),
            toJSON(list(b = 1, a = 2))
        )),
        unsorted
    )
    expect_equal(
        unclass(with(
            temp.options(crunch = list(crunch.stabilize.query = TRUE)),
            toJSON(list(b = 1, a = 2), for_query_string = TRUE)
        )),
        sorted
    )

})

test_that("setIfNotAlready", {
    with(temp.options(
        crunch.test.opt1 = "previous",
        crunch.test.opt2 = NULL,
        crunch.test.opt3 = 4
    ), {
        old <- setIfNotAlready(crunch.test.opt1 = "value", crunch.test.opt2 = 5)
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
    expect_identical(
        uniquify(rep("a", 4)),
        c("a", "a  (1)", "a  (2)", "a  (3)")
    )
    expect_identical(
        uniquify(c("b", "a", "a", "abcd", "a")),
        c("b", "a", "a  (1)", "abcd", "a  (2)")
    )
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
    with(reset.option("crunch.api"), {
        setCrunchAPI("foobar")
        expect_equal(getOption("crunch.api"), "https://foobar.crunch.io/api/")
        setCrunchAPI("barfoo", 8888)
        expect_equal(getOption("crunch.api"), "http://barfoo.crunch.io:8888/api/")
    })
})

with(temp.option(
    crunch = list(foo.crunch = "x"),
    foo.bar = "no",
    foo.other = "other",
    foo.crunch = "y"
), {
    withr::with_envvar(
        list(R_FOO_BAR = "yes", R_FOO_CRUNCH = "z", R_FOO_NUM = "1", R_FOO_LGL = "TRUE"),
        {
            test_that("envOrOption gets the right thing", {
                expect_identical(envOrOption("foo.crunch"), "x") ## crunch opt trumps all
                expect_identical(envOrOption("foo.bar"), "yes") ## Env var trumps option
                expect_identical(envOrOption("foo.other"), "other") ## Option if there is no env var
                expect_null(envOrOption("somethingelse")) ## Null if neither
                expect_equal(envOrOption("foo.num", expect_num = TRUE), 1)
                expect_equal(envOrOption("foo.lgl", expect_lgl = TRUE), TRUE)
                ## default works
                expect_identical(
                    envOrOption("somethingelse", "I'm a default"),
                    "I'm a default"
                )
            })
        }
    )
})

test_that("envOrOptionSource works correctly", {
    with(temp.option(
        crunch = list(
            crunch.opt = "a",
            crunch.opt.source = structure("b", source = "custom")
        ),
        opt = "c"
    ), {
        withr::with_envvar(list(R_ENV_VAR = "d"), {
            expect_equal(
                envOrOptionSource("crunch.opt"),
                "set using `set_crunch_opts(crunch.opt = ...)`"
            )
            expect_equal(
                envOrOptionSource("crunch.opt.source"),
                "set using `custom`"
            )
            expect_equal(
                envOrOptionSource("env.var"),
                "found in environment variable `R_ENV_VAR`"
            )
            expect_equal(
                envOrOptionSource("opt"),
                "found in `options(opt = ...)`"
            )
            expect_equal(
                envOrOptionSource("not.found"),
                "unknown source"
            )
        })
    })
})

test_that("loadCube can handle a number of locations", {
    # loadCube can load old fixtures that lack element/self/value metadata
    cube <- loadCube("cubes/array-cube-sans-metadata.json")
    expect_is(cube, "CrunchCube")
})

test_that("Cubify works with many dimensions", {
    # 1d
    cube_json <- fromJSON(cubePath("cubes/univariate-categorical.json"))
    cube <- loadCube("cubes/univariate-categorical.json")

    dn <- dimnames(cube@arrays$count)
    raw_values <- cube_json$value$result$measures$count$data
    expect_equivalent(cube@arrays$count, cubify(raw_values, dims = dn))
    expect_length(dim(cube@arrays$count), 1)

    # 2d
    cube_json <- fromJSON(cubePath("cubes/cat-array.json"))
    cube <- loadCube("cubes/cat-array.json")

    dn <- dimnames(cube@arrays$count)
    raw_values <- cube_json$value$result$measures$count$data
    expect_equivalent(cube@arrays$count, cubify(raw_values, dims = dn))
    expect_length(dim(cube@arrays$count), 2)

    # 3d
    cube_json <- fromJSON(cubePath("cubes/selected-crosstab-4.json"))
    cube <- loadCube("cubes/selected-crosstab-4.json")

    dn <- dimnames(cube@arrays$count)
    raw_values <- cube_json$value$result$measures$count$data
    expect_equivalent(cube@arrays$count, cubify(raw_values, dims = dn))
    expect_length(dim(cube@arrays$count), 3)

    # 3+d
    cube_json <- fromJSON(cubePath("cubes/cat-x-mr-x-mr.json"))
    cube <- loadCube("cubes/cat-x-mr-x-mr.json")

    dn <- dimnames(cube@arrays$count)
    raw_values <- cube_json$value$result$measures$count$data
    expect_equivalent(cube@arrays$count, cubify(raw_values, dims = dn))
    expect_length(dim(cube@arrays$count), 5)
})

test_that("is.TRUEorFALSE errors correctly", {
    expect_true(is.TRUEorFALSE(TRUE))
    expect_true(is.TRUEorFALSE(FALSE))
    expect_false(is.TRUEorFALSE("char"))
    expect_false(is.TRUEorFALSE(NA))
    expect_false(is.TRUEorFALSE(c(TRUE, TRUE)))
})

test_that("is.singleCharacter", {
    expect_true(is.singleCharacter("char"))
    expect_false(is.singleCharacter(c("char", "char2")))
    expect_false(is.singleCharacter(NULL))
    expect_false(is.singleCharacter(NA))
    expect_false(is.singleCharacter(1))
})

test_that("checkInstalledPackages", {
    expect_error(
        checkInstalledPackages(c("NoTaPaCkAgE", "NoRtHiSoNe")),
        paste0("Missing required packages: ", dQuote("NoTaPaCkAgE"), " and ", dQuote("NoRtHiSoNe"))
    )
    expect_silent(checkInstalledPackages("stats"))
})

test_that("hasFunction", {
    expect_true(hasFunction("makeArray", "crunch"))
    expect_false(hasFunction("Totally_not_a_function", "crunch"))
})

with_mock_crunch({
    ds <- cachedLoadDataset("test ds")

    test_that("haltIfArray", {
        expect_true(haltIfArray(ds$birthyr))
        expect_error(
            haltIfArray(ds$mymrset),
            "Array-like variables can't be used."
        )

        expect_error(haltIfArray(ds$mymrset, "embed_func()"),
            "Array-like variables can't be used with function `embed_func()`.",
            fixed = TRUE
        )
    })
})
