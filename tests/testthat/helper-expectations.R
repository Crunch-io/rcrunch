# nolint start
# we don't lint this file because we commit a few violations so that test envs
# can be set up and torn down easily.
expect_prints <- function(object, ..., fixed = TRUE) {
    expect_output(print(object), ..., fixed = fixed)
}

get_output <- function(x) {
    ## For comparing print output in expect_prints
    paste(capture.output(print(x)), collapse = "\n")
}

expect_deprecated <- function(...) expect_warning(..., "deprecated")

expect_valid_df_import <- function(ds) {
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
    expect_equivalent(
        as.array(crtabs(mean(v3) ~ v4, data = ds)),
        tapply(df$v3, df$v4, mean, na.rm = TRUE)
    )
    expect_equivalent(as.vector(ds$v3), df$v3)
    expect_true(is.Categorical(ds[["v4"]]))
    expect_equivalent(
        as.array(crtabs(~v4, data = ds)),
        array(c(10, 10), dim = 2L, dimnames = list(v4 = c("B", "C")))
    )
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

expect_valid_apidocs_import <- function(ds) {
    expect_true(is.dataset(ds))
    expect_identical(dim(ds), c(20L, 9L))
    expect_identical(
        names(ds),
        c(
            "allpets", "q1", "petloc", "ndogs", "ndogs_a", "ndogs_b", "q3",
            "country", "wave"
        )
    )
    expect_identical(name(ds), "Example dataset")
    expect_identical_temp_nodata(
        names(categories(ds$q1)),
        c("Cat", "Dog", "Bird", "Skipped", "Not Asked", "No Data")
    )
}

expect_identical_temp_nodata <- function(actual, expected) {
    # Newer versions of the Crunch API will automatically add a "No Data"
    # category if not present. This helper transitions us to that future, and
    # can be removed when all API nodes have moved to the new behavior. Replace
    # calls to this with `expect_identical(actual, expected)` once
    # https://www.pivotaltracker.com/story/show/164939686 is released.
    expect_true(
        identical(actual, expected) ||
            identical(actual, expected[expected != "No Data"])
    )
}

expect_equal_temp_nodata <- function(actual, expected) {
    # Newer versions of the Crunch API will automatically add a "No Data"
    # category if not present. This helper transitions us to that future, and
    # can be removed when all API nodes have moved to the new behavior. Replace
    # calls to this with `expect_equal(actual, expected)` once
    # https://www.pivotaltracker.com/story/show/164939686 is released.
    expect_true(
        isTRUE(all.equal(actual, expected)) ||
            isTRUE(all.equal(actual, expected[expected != -1]))
    )
}
# nolint end
