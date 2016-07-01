object_sort <- function (x) {
    if (is.list(x)) {
        x <- as.list(x) ## For S4 subclasses
        if (!is.null(names(x))) {
            x <- x[sort(names(x))]
        }
        return(lapply(x, object_sort))
    }
    return(x)
}

expect_json_equivalent <- function (object, expected, info = NULL,
                                    label = NULL, expected.label = NULL) {
    lab_act <- testthat:::make_label(object, label)
    lab_exp <- testthat:::make_label(expected, expected.label)
    comp <- json_compare(object, expected, check.attributes = FALSE)
    expect(comp$equal, sprintf("%s not JSON-equivalent to %s.\n%s",
        lab_act, lab_exp, comp$message), info = info)
    invisible(object)
}

json_compare <- function (object, expected, check.attributes = FALSE) {
    compare(object_sort(object), object_sort(expected),
        check.attributes = check.attributes)
}

expect_output <- function (object, ...) {
    testthat::expect_output(print(object), ...)
}

expect_length <- function(object, n) {
  stopifnot(is.numeric(n), length(n) == 1)
  lab <- testthat:::label(object)

  len <- length(object)

  expect(
    len == n,
    sprintf("%s has length %i, not length %i.", lab, length(object), n)
  )

  invisible(object)
}
