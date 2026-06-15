expect_prints <- function(object, ..., fixed = TRUE, crayon.enabled = NULL) {
    if (is.null(crayon.enabled)) {
        expect_output(print(object), ..., fixed = fixed)
    } else {
        expect_output(
            with(temp.option(crayon.enabled = crayon.enabled), print(object)),
            ...,
            fixed = fixed
        )
    }
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

expect_valid_df_revert <- function(ds) {
    ## Pull out common tests that "df" was reverted correctly
    ## This is different from `expect_valid_df_import` that it does Not
    ## expect the folder structure to have been reverted
    expect_true(is.dataset(ds))
    expect_identical(description(ds), "")
    expect_true(setequal(names(df), names(ds)))
    expect_identical(dim(ds), dim(df))
    expect_true(is.Numeric(ds[["v1"]]))
    expect_true(is.Text(ds[["v2"]]))
    expect_identical(name(ds$v2), "v2")
    expect_true(is.Numeric(ds[["v3"]]))
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
    expect_identical(names(versions(ds)), "initial import")
}

expect_valid_apidocs_import <- function(ds, broken_exclusion = FALSE) {
    expect_true(is.dataset(ds))
    if (broken_exclusion) {
        expect_identical(dim(ds), c(0L, 9L))
    } else {
        expect_identical(dim(ds), c(20L, 9L))
    }
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

## Moving from using variable URL to var alias in ZCL
## Can't use `expect_json_equivalent` because we have 2 conditions
## so make a simple verison here:
expect_zcl_equivalent <- function(actual, expected) {
    env <- parent.frame()
    call <- sys.call()
    actual_expr <- call[[2]]
    expected_expr <- call[[3]]
    with(temp.options(crunch = list(crunch.alias.zcl = FALSE)), {
        actual_url_zcl <- object_sort(zcl(eval(actual_expr, env)))
        expected_url_zcl <- object_sort(zcl(eval(expected_expr, env)))
    })
    if (isTRUE(all.equal(actual_url_zcl, expected_url_zcl))) {
        return(expect_true(TRUE))
    }

    with(temp.options(crunch = list(crunch.alias.zcl = TRUE)), {
        actual_var_zcl <- object_sort(zcl(eval(actual_expr, env)))
        expected_var_zcl <- object_sort(zcl(eval(expected_expr, env)))
    })
    if (isTRUE(all.equal(actual_var_zcl, expected_var_zcl))) {
        return(expect_true(TRUE))
    }

    # No match, but don't use httptest::expect_json_equivlent
    # because the output is terrible

    stop(paste0(
        "ZCLs not equivalent:\n",
        "---Actual (url)\n", toJSON(actual_url_zcl), "\n",
        "---Expected (url)\n", toJSON(expected_url_zcl), "\n",
        "---Actual (alias)\n", toJSON(actual_var_zcl), "\n",
        "---Expected (alias)\n", toJSON(expected_var_zcl)
    ))
}

object_sort <- function (x) {
    if (is.list(x)) {
        x <- as.list(x)
        if (!is.null(names(x))) {
            x <- x[sort(names(x))]
        }
        return(lapply(x, object_sort))
    }
    return(x)
}

