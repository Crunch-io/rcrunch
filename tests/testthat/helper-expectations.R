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

expect_json_equivalent <- function (object, expected, ...) {
    expect_equivalent(object_sort(object), object_sort(expected), ...)
}

expect_output <- function (object, ...) {
    testthat::expect_output(print(object), ...)
}

expect_length <- function(object, n) {
  stopifnot(is.numeric(n), length(n) == 1)
  lab <- testthat:::label(object)

  ## Forked from testthat because their version whitelists types and excludes
  ## S4 objects even if they have a length method defined
  len <- length(object)

  expect(
    len == n,
    sprintf("%s has length %i, not length %i.", lab, length(object), n)
  )

  invisible(object)
}

expect_mock_request <- function (object, ...) {
    ## With mock HTTP, POST/PUT/PATCH throw errors with their request info
    expect_error(object, paste0(...), fixed=TRUE)
}
