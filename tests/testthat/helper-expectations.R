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
