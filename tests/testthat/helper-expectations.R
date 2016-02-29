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

json_equivalent <- function(expected, label = NULL, ...) {
  if (is.null(label)) {
    label <- testthat:::find_expr("expected")
  } else if (!is.character(label) || length(label) != 1) {
    label <- deparse(label)
  }

  function(actual) {
    same <- compare(object_sort(actual), object_sort(expected),
      check.attributes=FALSE, ...)

    expectation(
      same$equal,
      paste0("not JSON-equivalent to ", label, "\n", same$message),
      paste0("JSON-equals ", label)
    )
  }
}

expect_json_equivalent <- function(object, expected, ..., info = NULL, label = NULL,
  expected.label = NULL) {
  if (is.null(label)) {
    label <- testthat:::find_expr("object")
  }
  if (is.null(expected.label)) {
    expected.label <- testthat:::find_expr("expected")
  }
  expect_that(object, json_equivalent(expected, label = expected.label, ...),
    info = info, label = label)
}

does_not_give_warning <- function () {
    function (expr) {
        warnings <- evaluate_promise(expr)$warnings
        expectation(length(warnings) == 0,
                paste0(length(warnings), " warnings created"),
                "no warnings given")
    }
}

does_not_show_message <- function () {
    function (expr) {
        warnings <- evaluate_promise(expr)$messages
        expectation(length(messages) == 0,
                paste0(length(messages), " messages created"),
                "no messages shown")
    }
}

does_not_throw_error <- function () {
    function (expr) {
        res <- try(force(expr), TRUE)
        error <- inherits(res, "try-error")
        failure.msg <- "threw an error"
        if (error) {
            ## Append the error message
            failure.msg <- paste0(failure.msg, ": ",
                attr(res, "condition")$message)
        }
        expectation(!error, failure.msg, "no error thrown")
    }
}

is_not_an_error <- function () {
    ## Like does_not_throw_error, but for an error already caught
    function (expr) {
        expectation(!is.error(expr),
            paste("is an error:", attr(expr, "condition")$message),
            "no error thrown")
    }
}
