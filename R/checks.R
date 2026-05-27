check_collect <- function(message, ..., call = rlang::caller_env(), severity = c("error", "warning")) {
    severity <- rlang::arg_match(severity)

    errors <- purrr::map(
        rlang::enquos(...),
        function(check) tryCatch(rlang::eval_tidy(check), error = function(e) e)
    ) |>
        purrr::keep(rlang::is_error) |>
        purrr::map(function(e) {
            if (rlang::cnd_inherits(e, "crunch_lake_collect")) {
                if (is.null(e$raw_message)) {
                    e$errors
                } else {
                     setNames(list(e$errors), e$raw_message)
                }
            } else {
                conditionMessage(e)
            }
        })

    errors <- unlist(errors, recursive = FALSE)

    if (length(errors) == 0) return()

    alert_fn <- if (severity == "error") rlang::abort else rlang::warn
    alert_fn(
        message = message %||% "Check failed",
        errors = errors,
        raw_message = message,
        call = call,
        class = "crunch_lake_collect"
    )

}

#' @importFrom rlang cnd_body
#' @export
cnd_body.crunch_lake_collect <- function(cnd, ...) {
    format_error_bullets_indented(cnd$errors)
}

format_error_bullets_indented <- function(err, indent = 0) {
    indent_text <- paste0(rep(" ", indent), collapse = "")
    if (rlang::is_error(err)) {
        return(paste0(indent_text, gsub("\n", paste0("\n", indent_text), conditionMessage(err), fixed = TRUE)))
    }
    messages <- lapply(seq_along(err), function(iii) {
        x <- err[[iii]]
       if (is.null(names(err)) || names(err)[iii] == "") {
            unindented <- rlang::format_error_bullets(setNames(x, rep("x", length(x))))
            paste0(indent_text, gsub("\n", paste0("\n", indent_text), unindented, fixed = TRUE))
        } else {
            c(
                paste0(indent_text, rlang::format_error_bullets(c("x" = names(err)[iii]))),
                format_error_bullets_indented(x, indent = indent + 2)
            )
        }
    }) |> unlist() |>
        paste(collapse = "\n")
}

check_if <- function(pred, ..., message = NULL, call = rlang::caller_env()) {
    pred

    check_collect(
        message,
        ...,
        call = call
    )
}

check_inherits <- function(x, what, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
    if (!inherits(x, what)) {
        cli::cli_abort(
            "{.arg {arg}} must be {.cls {what}} (got {.obj_type_friendly {x}})",
            call = call
        )
    }
    invisible(x)
}

check_single_string <- function(x, null_ok = FALSE, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
    if (null_ok && (is.null(x) || length(x) == 0)) return(invisible(NULL))
    check_inherits(x, "character", arg = arg)
    if (length(x) != 1) {
        cli::cli_abort(
            "{.arg {arg}} must be length 1 (got length {length(x)})",
            call = call
        )
    }
    invisible(x)
}

check_df_has <- function(...) {
    check_has(..., item_identifier = "column")
}

check_has <- function(x, item, item_identifier = "item", arg = rlang::caller_arg(x), call = rlang::caller_env()) {
    if (!item %in% names(x)) cli::cli_abort(
        "{.arg {arg}} does not have {item_identifier} named {item}",
        call = call
    )
    invisible(x)
}

check_unique <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
    if (is.data.frame(x)) {
        x <- purrr::pmap_chr(x, function(...) {
            dots <- list(...)
            paste0(names(dots), "=", dots, collapse = " ")
        })
    }
    x_non_missing <- x[!is.na(x)]
    if (anyDuplicated(x_non_missing)) {
        dups <- truncated_list(x_non_missing[duplicated(x_non_missing)])
        cli::cli_abort(
            "{.arg {arg}} has duplicated items ({dups})",
            call = call
        )
    }

    invisible(x)
}

check_all_in <- function(x, y, na_ok = TRUE, x_arg = rlang::caller_arg(x), y_arg = rlang::caller_arg(y), call = rlang::caller_env()) {
    if (is.data.frame(x)) {
        x_values <- lapply(x, unique) |> unlist() |> unique()
    } else {
        x_values <- unique(x)
    }
    if (!all(x_values %in% y | (na_ok & is.na(x_values)))) {
        extras <- truncated_list(setdiff(x_values, c(y, if(na_ok) NA)))
        cli::cli_abort(
            "{x_arg} not entirely in {.arg {y_arg}} (extra items: {extras})",
            call = call
        )
    }
    invisible(x)
}

check_nonmissing <- function(x, x_arg = rlang::caller_arg(x), call = rlang::caller_env()) {
    check_true(all(!is.na(x)), "{.arg {x_arg}} not allowed to have missing values", x_arg = x_arg, call = call)
    invisible(x)
}


check_true <- function(pred, ..., call = rlang::caller_env(), .envir = parent.frame()) {
    if (!pred) cli::cli_abort(..., call = call, .envir = .envir)
}

truncated_list <- function(x) {
    x <- unique(x)
    if (length(x) > 3) x <- c(x[1:3], cli::symbol$ellipsis)
    paste(x, collapse = ", ")
}

pmap_check <- function(.x, .f, ..., .message = NULL, call = rlang::caller_env()) {
    map_func <- purrr::as_mapper(.f, ...)
    purrr::pmap(.x, function(...) {
        out <- try(map_func(...), silent = TRUE)
        if (inherits(out, "try-error")) {
            err <- attr(out, "condition")
            if (!is.null(.message)) {
                message <- cli::format_inline(.message, .envir = as.environment(list(...)))
                err <- rlang::error_cnd(
                    class = "crunch_lake_collect",
                    message = message,
                    raw_message = message,
                    errors = if (inherits(err, "crunch_lake_collect")) setNames(list(err$errors), err$message) else err,
                    call = call
                )
            }
            return(err)
        }
        out
    })
}
