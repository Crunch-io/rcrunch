context("Various helper functions")

test_that("is.error", {
    expect_true(is.error(try(halt(""), silent=TRUE)))
    expect_false(is.error("not an error"))
    expect_false(is.error(NULL))
    expect_false(is.error(NA))
    expect_that("not an error", does_not_throw_error())
})

test_that("update list", {
    a <- list(a=1, b=2)
    b <- list(c=3, b=4)
    expect_identical(updateList(a, b), list(a=1, b=4, c=3))
    expect_identical(updateList(list(), b), b)
    expect_identical(updateList(NULL, b), b)
})

test_that("selectFrom selects what it should", {
    l1 <- list(list(a=1, b=2), list(c=3, b=4))
    expect_identical(selectFrom("b", l1), c(2, 4))
    expect_identical(selectFrom("a", l1), c(1, NA))
    expect_identical(selectFrom("a", l1, ifnot=4), c(1, 4))
    expect_identical(selectFrom("d", l1), c(NA, NA))
    l2 <- l1
    l2[[2]] <- 4
    expect_identical(selectFrom("b", l2), c(2, NA))
    expect_error(selectFrom("b", 5), "xlist must be a list object")
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
    
    a <- NULL
    expect_true(is.null(a))
    with(tester, {
        expect_false(is.null(a))
        expect_false(a)
        halt("Testing error handling, please ignore")
    })
    expect_true(a)
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

test_that("encoding", {
    s <- iconv("aided_follow_grid:ElCorteInglés", to="UTF-8")
    expect_identical(Encoding(s), "UTF-8")
    expect_true(grepl("Inglés", s))
    sj <- toJSON(s)
    expect_true(grepl("Inglés", sj))
    s2 <- fromJSON(sj)
    expect_identical(s2, s)
})

skip(test_that("encoding and parseJSONResponse", {
    print(fromJSON("utf-test.json",
        simplifyWithNames=FALSE, encoding="UTF-8"))
    print(mungeEncoding(fromJSON("utf-test.json",
        simplifyWithNames=FALSE)))
    expect_identical(mungeEncoding(fromJSON("utf-test.json",
        simplifyWithNames=FALSE)), 
        "Budějovický Budvar")
}))

test_that("joinPath", {
    expect_identical(joinPath("/api/datasets/", "../variables/"),
        "/api/variables/")
    expect_identical(joinPath("/api/variables/", "4412es.json"),
        "/api/variables/4412es.json")
    expect_identical(joinPath("a/b/c/d/../e/f/", "g/../../h/"),
        "a/b/c/e/h/")
    expect_identical(joinPath("/api/datasets/", "/variables/"),
        "/variables/")
})

if (run.integration.tests) {
    with(test.authentication, {
        with(test.dataset(df), {
            s <- iconv("aided_follow_grid:ElCorteInglés", to="UTF-8")
            name(ds$v1) <- s
            expect_identical(name(ds$v1), s)
            expect_identical(name(refresh(ds)$v1), s)
        })
    })
}