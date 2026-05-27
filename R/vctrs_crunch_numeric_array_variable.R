#' Create Crunch Numeric Array Variable
#'
#' Crunch numeric array variables are kind of like a "packed" data.frame of numeric vectors
#' but with extra metadata. TODO: WRITE MORE!
#'
#' @param x A data.frame of values data
#' @param label The variable's label
#' @param axes A data.frame of axes metadata (`NULL`, the default requires that x be a
#' data.frame of crunch_variables, which have the necessary metadata)
#' @param description The variable's description
#' @param notes The variable's notes
#' @param path The variable's folder path
#' @param ... Ignored
#' @returns A `crunch_numeric_array_variable` object
#' @export
crunch_numeric_array_variable <- function(
        x,
        label,
        axes = NULL,
        description = NULL,
        notes = NULL,
        path = NULL,
        ...
) {
    check_collect(
        "Invalid crunch_numeric_array_variable",
        check_single_string(label),
        check_single_string(description, null_ok = TRUE),
        check_single_string(notes, null_ok = TRUE),
        check_single_string(path, null_ok = TRUE),
        x <<- validate_axes(axes, x, type = "numeric")
    )

    new_crunch_numeric_array_variable(
        x = x,
        label = label,
        description = description,
        notes = notes,
        path = path
    )
}

new_crunch_numeric_array_variable <- function(
        x,
        label,
        description = NULL,
        notes = NULL,
        path = NULL,
        ...
) {
    # Don't want to inherit from vctrs, this should mostly be treated as a data.frame
    # not as a singular unit
    x <- tibble::as_tibble(x)
    out <- structure(
        x,
        label = label,
        description = description,
        notes = notes,
        path = path,
        class = c("crunch_array_variable", "crunch_variable", class(x))
    )

    # Augment to numeric
    class(out) <- c("crunch_numeric_array_variable", class(out))

    out
}

#' @export
is.crunch_numeric_array_variable <- function(x) {
    inherits(x, "crunch_numeric_array_variable")
}


#' @export
vec_ptype_abbr.crunch_numeric_array_variable <- function(x, ...) {
    "cr_num_arr"
}

# ---- Attributes -----
# None extra

# ---- Coercion -----
# Handled by crunch_array_variable

# ----- Subsetting / Sub assigning -----
# Base methods handled by crunch_array_variable

# dynamic export; see zzz.R
dplyr_reconstruct.crunch_numeric_array_variable <- function(data, template) {
    out <- NextMethod()
    check_collect(
        "Invalid array",
        out <<- validate_axes(NULL, out, "numeric")
    )
    out
}

#' @export
vec_cast.crunch_numeric_array_variable.crunch_numeric_array_variable <- function(x, to, x_arg = rlang::caller_env(), ...) {
    check_collect(
        message = "Cannot convert to comparable crunch_numeric_array_variable",
        x <<- validate_axes(axes(to), x, type = "numeric", x_arg = x_arg)
    )

    return(x)
}

#' @export
vec_cast.crunch_numeric_array_variable.data.frame <- function(
        x, to, x_arg = rlang::caller_arg(x), to_arg = rlang::caller_arg(to), call = rlang::caller_env(), ...
) {
    check_collect(
        message = "Cannot convert to comparable crunch_numeric_array_variable",
        x <<- validate_axes(axes(to), x, type = "numeric", x_arg = x_arg)
    )
    new_crunch_numeric_array_variable(
        x,
        label = label(to),
        description = description(to),
        notes = notes(to),
        path = path(to)
    )
}

#' @export
vec_cast.crunch_numeric_array_variable.tbl_df <- vec_cast.crunch_numeric_array_variable.data.frame
