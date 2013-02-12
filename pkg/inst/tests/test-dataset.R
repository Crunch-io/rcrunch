context("Dataset object and methods") 

test_that("Dataset can be created", {
    expect_equivalent(class(CrunchDataset(name="test ds")), "CrunchDataset")
    expect_true(is.dataset(CrunchDataset(name="test ds")))
    expect_identical(CrunchDataset(name="test ds")@name, "test ds")
})

test_that("Dataset getters get", {
    expect_identical(name(CrunchDataset(name="test ds")),
        CrunchDataset(name="test ds")@name)
})

test_that("Datasets only contain variables", {
    d1 <- CrunchDataset(name="test ds")
    var_test_fn <- function (x) all(vapply(x, is.variable, logical(1)))
    expect_true(var_test_fn(d1))
    expect_error(CrunchDataset(list(foo=34), name="test ds"), 
        ".*1 element is not a Crunch variable object.")
})

test_that("Can construct Dataset from shoji document", {
    sho <- fromJSON(system.file("dataset.json", package="rcrunch", mustWork=TRUE), simplifyWithNames=FALSE)
    expect_true(is.dataset(.cr.dataset.shoji(sho)))
    expect_true(is.dataset(as(sho, "CrunchDataset")))
    expect_true(is.dataset(as.dataset(sho)))
})