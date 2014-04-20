context("Various helper functions")

test_that("is.error", {
    expect_true(is.error(try(stop(""), silent=TRUE)))
    expect_false(is.error("not an error"))
    expect_false(is.error(NULL))
    expect_false(is.error(NA))
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
    tester <- setup.and.teardown(function () a <<- FALSE, function () a <<- TRUE)
    
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
        stop(call.=FALSE)
    })
    expect_true(a)
})