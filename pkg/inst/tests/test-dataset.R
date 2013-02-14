context("Dataset object and methods") 

test_that("Dataset can be created", {
    expect_equivalent(class(CrunchDataset(body=list(name="test ds"))), "CrunchDataset")
    expect_true(is.dataset(CrunchDataset(body=list(name="test ds"))))
    expect_identical(CrunchDataset(body=list(name="test ds"))@body$name, "test ds")
})

test_that("Dataset getters get", {
    expect_identical(name(CrunchDataset(body=list(name="test ds"))),
        CrunchDataset(body=list(name="test ds"))@body$name)
})

var_test_fn <- function (x) all(vapply(x, is.variable, logical(1)))

test_that("Datasets only contain variables", {
    d1 <- CrunchDataset(body=list(name="test ds"))
    expect_true(var_test_fn(d1))
    expect_error(CrunchDataset(list(foo=34), body=list(name="test ds")), 
        ".*1 element is not a Crunch variable object.")
})

## Dataset fixture
ds <- fromJSON(system.file("dataset.json", package="rcrunch", 
    mustWork=TRUE), simplifyWithNames=FALSE)
class(ds) <- "shoji"

test_that("Can construct Dataset from shoji document", {
    sho <- ds
    sh2 <- as.shojiObject(sho)
    expect_true(is.dataset(.cr.dataset.shojiObject(sh2)))
    expect_true(is.dataset(as(sh2, "CrunchDataset")))
    expect_true(is.dataset(as(sho, "CrunchDataset")))
    expect_true(is.dataset(as.dataset(sh2)))
    expect_true(is.dataset(as.dataset(sho)))
})

## Variable fake fixtures
vars <- fromJSON(system.file("variables.json", package="rcrunch",
    mustWork=TRUE), simplifyWithNames=FALSE)
names(vars) <- selectFrom("alias", vars)
vars <- lapply(vars, function (x) structure(list(body=x), class="shoji"))
vars <- lapply(vars, as.variable)

test.ds <- .cr.dataset.shojiObject(as.shojiObject(ds), vars)

test_that("A dataset with variables inherits from list", {
    expect_identical(names(test.ds), names(vars))
    expect_true(is.variable(test.ds[[1]]))
    expect_true(is.variable(test.ds$age))
    expect_true(is.dataset(test.ds[1])) ## fails
    expect_identical(test.ds$age, vars$age)
    expect_true(var_test_fn(test.ds))
})

test_that("Read only flag gets set appropriately", {
    expect_false(is.readonly(test.ds))
    expect_true(is.readonly(test.ds[1]))
})