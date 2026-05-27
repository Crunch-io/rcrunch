typed_crunch_variable <- function(x, type, axes, ...) {
    if (type == "categorical" && is.null(axes)) {
        crunch_categorical_variable(x = x, ...)
    } else if (type == "numeric" && is.null(axes)) {
        crunch_numeric_variable(x = x, ...)
    } else if (type == "datetime") {
        crunch_datetime_variable(x = x, ...)
    } else if (type == "text") {
        crunch_text_variable(x = x, ...)
    } else if (type == "categorical" && !is.null(axes)) {
        crunch_categorical_array_variable(x = x, axes = axes, ...)
    } else if (type == "numeric" && !is.null(axes)) {
        crunch_numeric_array_variable(x = x, axes = axes, ...)
    } else {
        has_axes <- if (is.null(axes)) "no axes defined" else "with axes"
        cli::cli_abort("Unexpected variable structure {.type {type}} & {has_axes}")
    }
}

new_crunch_variable <- function(x, label, description = NULL, notes = NULL, path = NULL, inherit_base_type = NULL) {
    vctrs::new_vctr(
        x,
        label = label,
        description = description,
        notes = notes,
        path = path,
        class = "crunch_variable",
        inherit_base_type = inherit_base_type
    )
}

is.crunch_variable <- function(x) {
    inherits(x, "crunch_variable")
}

# ---- Formatting ----
#' @export
obj_print_header.crunch_variable <- function(x, ...) {
    cli::cat_line("<", vctrs::vec_ptype_full(x), "[", vctrs::vec_size(x), "]>", paste0(": ", label(x)))
    invisible(x)
}

# ---- Attributes ----
# crunch_variable isn't ever actually the final class, so don't have to define
# the full set of vctrs functions
# Use it to have a common place to set crunch common metadata

#' @export
label.crunch_variable <- function(x, ...) {
    attr(x, "label", exact = TRUE)
}

#' @export
description.crunch_variable <- function(x, ...) {
    attr(x, "description", exact = TRUE)
}

#' @export
notes.crunch_variable <- function(x, ...) {
    attr(x, "notes", exact = TRUE)
}

#' @export
path.crunch_variable <- function(x, ...) {
    attr(x, "path", exact = TRUE)
}

#' @export
set_label.crunch_variable <- function(x, value, ...) {
    check_single_string(value)
    attr(x, "label") <- value
    x
}

#' @export
set_description.crunch_variable <- function(x, value, ...) {
    check_single_string(value, null_ok = TRUE)
    attr(x, "description") <- value
    x
}

#' @export
set_notes.crunch_variable <- function(x, value, ...) {
    check_single_string(value, null_ok = TRUE)
    attr(x, "notes") <- value
    x
}

#' @export
set_path.crunch_variable <- function(x, value, ...) {
    check_single_string(value, null_ok = TRUE)
    attr(x, "path") <- value
    x
}

# ---- Coercion ----

#' @export
as.character.crunch_variable <- function(x, ...) {
    as.character(vctrs::vec_data(x), ...)
}

#' @export
as.double.crunch_variable <- function(x, ...) {
    as.double(vctrs::vec_data(x), ...)
}

#' @export
as.Date.crunch_variable <- function(x, ...) {
    as.Date(vctrs::vec_data(x), ...)
}

#' @export
as.POSIXct.crunch_variable <- function(x, ...) {
    as.POSIXct(vctrs::vec_data(x), ...)
}

#' @export
as.POSIXlt.crunch_variable <- function(x, ...) {
    as.POSIXlt(vctrs::vec_data(x), ...)
}
