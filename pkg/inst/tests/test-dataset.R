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
is_valid_dataset <- function (x) is.dataset(x) && var_test_fn(x)

test_that("Datasets only contain variables", {
    d1 <- CrunchDataset(body=list(name="test ds"))
    expect_true(is_valid_dataset(d1))
    expect_error(CrunchDataset(list(foo=34), body=list(name="test ds")), 
        ".*1 element is not a Crunch variable object.")
})

## Uses structures from helper.R
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
test.ds <- .cr.dataset.shojiObject(as.shojiObject(ds), vars2)

test_that("Dataset has names() and extract methods work", {
    expect_false(is.null(names(test.ds)))
    expect_identical(names(test.ds), names(vars2))
    expect_true(is.variable(test.ds[[1]]))
    expect_true("age" %in% names(test.ds))
    expect_true(is.variable(test.ds$age))
    expect_true(is.dataset(test.ds[1]))
    expect_identical(test.ds$age, vars2$age)
    expect_true(is_valid_dataset(test.ds))
    expect_true(is_valid_dataset(test.ds[1]))
    expect_true(is_valid_dataset(test.ds["age"]))
    expect_true(is_valid_dataset(test.ds[names(test.ds)=="age"]))
})

test_that("Read only flag gets set appropriately", {
    expect_false(is.readonly(test.ds))
    expect_true(is.readonly(test.ds[1]))
})

test_that("Name and description setters in read only mode", {
    dataset <- test.ds
    readonly(dataset) <- TRUE
    name(dataset) <- "Bond. James Bond."
    expect_false(identical(name(dataset), name(test.ds)))
    expect_identical(name(dataset), "Bond. James Bond.")
    description(dataset) <- "007"
    expect_false(identical(description(dataset), description(test.ds)))
    expect_identical(description(dataset), "007")
})

if (!run.only.local.tests) {
    with(test.authentication, {
        with(test.dataset(df, "setter_test"), {
            test_that("Name and description setters push to server", {
                d2 <- setter_test <- .setup
                name(setter_test) <- "Bond. James Bond."
                expect_identical(name(refresh(d2)), name(setter_test))
            })
        })
        test_that("Name and description setters don't push to server if readonly", {

        })
    })
}