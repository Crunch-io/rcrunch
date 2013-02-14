context("Variables")

vars <- fromJSON(system.file("variables.json", package="rcrunch",
    mustWork=TRUE), simplifyWithNames=FALSE)
vars <- lapply(vars, function (x) structure(list(body=x), class="shoji"))

test_that("Variable init, as, is", {
    expect_true(is.variable(do.call("CrunchVariable", vars[[1]])))
    expect_true(is.variable(as.variable(vars[[1]])))
    expect_true(is.variable(as.variable(as.shojiObject(vars[[1]]))))
    expect_true(all(vapply(lapply(vars, as.variable), is.variable, logical(1))))
    expect_false(is.variable(5))
    expect_false(is.variable(NULL))
})