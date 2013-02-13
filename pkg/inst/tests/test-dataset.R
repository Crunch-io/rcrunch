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

test_that("Datasets only contain variables", {
    d1 <- CrunchDataset(body=list(name="test ds"))
    var_test_fn <- function (x) all(vapply(x, is.variable, logical(1)))
    expect_true(var_test_fn(d1))
    expect_error(CrunchDataset(list(foo=34), body=list(name="test ds")), 
        ".*1 element is not a Crunch variable object.")
})

test_that("Can construct Dataset from shoji document", {
    sho <- fromJSON(system.file("dataset.json", package="rcrunch", mustWork=TRUE), simplifyWithNames=FALSE)
    class(sho) <- "shoji"
    sh2 <- as.shojiObject(sho)
    expect_true(is.dataset(.cr.dataset.shojiObject(sh2)))
    expect_true(is.dataset(as(sh2, "CrunchDataset")))
    expect_true(is.dataset(as(sho, "CrunchDataset")))
    expect_true(is.dataset(as.dataset(sh2)))
    expect_true(is.dataset(as.dataset(sho)))
})