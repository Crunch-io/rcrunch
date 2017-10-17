expect_output <- function (object, ...) {
    testthat::expect_output(print(object), ...)
}

expect_fixed_output <- function (object, ...) {
    expect_output(object, ..., fixed=TRUE)
}

get_output <- function (x) {
    ## For comparing print output in expect_output
    paste(capture.output(print(x)), collapse="\n")
}

expect_length <- function(object, n) {
  stopifnot(is.numeric(n), length(n) == 1)
  lab <- deparse(object)

  ## Forked from testthat because their version whitelists types and excludes
  ## S4 objects even if they have a length method defined
  len <- length(object)

  expect(
    len == n,
    sprintf("%s has length %i, not length %i.", lab, length(object), n)
  )

  invisible(object)
}

expect_valid_df_import <- function (ds) {
    ## Pull out common tests that "df" was imported correctly
    expect_true(is.dataset(ds))
    expect_identical(description(ds), "")
    expect_identical(names(df), names(ds))
    expect_identical(dim(ds), dim(df))
    expect_true(is.Numeric(ds[["v1"]]))
    expect_true(is.Text(ds[["v2"]]))
    expect_identical(name(ds$v2), "v2")
    expect_true(is.Numeric(ds[["v3"]]))
    expect_identical(description(ds$v3), "")
    expect_equivalent(as.array(crtabs(mean(v3) ~ v4, data=ds)),
        tapply(df$v3, df$v4, mean, na.rm=TRUE))
    expect_equivalent(as.vector(ds$v3), df$v3)
    expect_true(is.Categorical(ds[["v4"]]))
    expect_equivalent(as.array(crtabs(~ v4, data=ds)),
        array(c(10, 10), dim=2L, dimnames=list(v4=c("B", "C"))))
    expect_true(all(levels(df$v4) %in% names(categories(ds$v4))))
    expect_identical(categories(ds$v4), categories(refresh(ds$v4)))
    expect_identical(ds$v4, refresh(ds$v4))
    expect_equivalent(as.vector(ds$v4), df$v4)
    expect_true(is.Datetime(ds$v5))
    expect_true(is.Categorical(ds$v6))
    expect_identical(names(categories(ds$v6)), c("True", "False", "No Data"))
    expect_identical(showShojiOrder(ordering(ds)), names(variables(ds)))
    expect_identical(names(versions(ds)), "initial import")
}

expect_valid_apidocs_import <- function (ds) {
    expect_true(is.dataset(ds))
    expect_identical(dim(ds), c(20L, 9L))
    expect_identical(names(ds),
        c("allpets", "q1", "petloc", "ndogs", "ndogs_a", "ndogs_b", "q3",
        "country", "wave"))
    expect_identical(name(ds), "Example dataset")
    expect_identical(names(categories(ds$q1)),
        c("Cat", "Dog", "Bird", "Skipped", "Not Asked"))
}
