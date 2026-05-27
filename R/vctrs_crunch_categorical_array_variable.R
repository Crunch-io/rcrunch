#' Create Crunch Categorical Array Variable
#'
#' Crunch categorical array variables are kind of like a "packed" data.frame of factors
#' but with extra metadata. TODO: WRITE MORE!
#'
#' @param x A data.frame of values data
#' @param label The variable's label
#' @param values A data.frame of values metadata
#' @param axes A data.frame of axes metadata (`NULL`, the default requires that x be a
#' data.frame of crunch_variables, which have the necessary metadata)
#' @param description The variable's description
#' @param notes The variable's notes
#' @param path The variable's folder path
#' @param ... Ignored
#' @returns A `crunch_categorical_array_variable` object
#' @export
crunch_categorical_array_variable <- function(
        x,
        label,
        values = NULL,
        axes = NULL,
        description = NULL,
        notes = NULL,
        path = NULL,
        ...
) {
    check_collect(
        "Invalid crunch_categorical_array_variable",
        check_single_string(label),
        check_single_string(description, null_ok = TRUE),
        check_single_string(notes, null_ok = TRUE),
        check_single_string(path, null_ok = TRUE),
        values <<- validate_array_category_data(values, x),
        x <<- validate_category_value_data(x),
        x <<- validate_axes(axes, x, type = "categorical", values = values)
    )

    new_crunch_categorical_array_variable(
        x = x,
        label = label,
        values = values,
        description = description,
        notes = notes,
        path = path
    )
}

new_crunch_categorical_array_variable <- function(
        x,
        label,
        values,
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

    # Augment to categorical
    attr(out, "values")  <- values
    class(out) <- c("crunch_categorical_array_variable", class(out))

    out
}

validate_array_category_data <- function(values, x, call = rlang::caller_env()) {
    crunch_var_types <- c("crunch_categorical_variable", "crunch_categorical_array_variable")
    all_var_types <- purrr::map(x, class)
    x_is_crunch_var <- purrr::map_lgl(all_var_types, ~any(. %in% crunch_var_types))

    if (is.null(values)) {
        check_collect(
            "Invalid data for creating a categorical array variable without specifying values",
            check_true(
                all(x_is_crunch_var),
                "All columns must be crunch variables of type {.or {.arg {crunch_var_types}}}",
                call = call
            ) |>
                check_if(check_true(
                    length(unique(purrr::map(x, ~values(.)))) == 1,
                    "All columns must have identical category values",
                    call = call
                ))
        )
        return(values(x[[1]]))
    } else if (any(x_is_crunch_var)) {
        # Validate category values enough to force types, but assume data is already validated
        values <- validate_category_values(values, character(0), call = call)
        check_collect(
            "Invalid data for creating a categorical array from existing crunch variables",
            check_true(
                all(x_is_crunch_var),
                "All columns must be crunch variables of type {.or {crunch_var_types}}",
                call = call
            ),
            check_true(
                identical(values, unique(purrr::map(x, ~values(.)))[[1]]),
                "All columns must have category values that match the values argument",
                call = call
            )
        )
        return(values)
    } else {
        validate_category_values(values, x, call = call)
    }
}

#' @export
is.crunch_categorical_array_variable <- function(x) {
    inherits(x, "crunch_categorical_array_variable")
}

#' @export
vec_ptype_abbr.crunch_categorical_array_variable <- function(x, ...) {
    "cr_cat_arr"
}


# ---- Attributes -----
#' @export
values.crunch_categorical_array_variable <- function(x, ...) {
    attr(x, "values", exact = TRUE)
}

#' @export
set_values.crunch_categorical_array_variable <- function(x, value, validate = TRUE, ...) {
    attr(x, "values") <- value
    if (validate) {
        value <- validate_category_values(value, vec_data(x))
    }
    orig_x <- x
    x <- vec_data(x)
    x[] <- lapply(x, function(item) {
        set_values(item, value, validate = FALSE)
    })
    vec_restore(x, orig_x)
}

# ---- Coercion -----
# Handled by crunch_array_variable


# ----- Subsetting/Assigning into -----
# base methods handled by crunch_array_var

# dynamic export; see zzz.R
dplyr_reconstruct.crunch_categorical_array_variable <- function(data, template) {
    out <- NextMethod()
    check_collect(
        "Invalid array",
        validate_array_category_data(NULL, data),
        out <<- validate_axes(NULL, out, "categorical", values = values(template))
    )
    out
}

#' @export
vec_cast.crunch_categorical_array_variable.crunch_categorical_array_variable <- function(x, to, x_arg = rlang::caller_env(), ...) {
    values <- values(to)
    check_collect(
        message = "Cannot convert to comparable crunch_categorical_array_variable",
        values <<- coalesce_categorical_values(x, to, x_arg = x_arg, y_arg = to_arg, action = "convert"),
        x <<- validate_axes(axes(to), x, type = "categorical", values = values, x_arg = x_arg)
    )

    return(x)
}

#' @export
vec_cast.crunch_categorical_array_variable.data.frame <- function(
        x, to, x_arg = rlang::caller_arg(x), to_arg = rlang::caller_arg(to), call = rlang::caller_env(), ...
) {
    values <- values(to)
    check_collect(
        message = "Cannot convert to comparable crunch_categorical_array_variable",
        x <<- validate_category_value_data(x, values(to)),
        x <<- validate_axes(axes(to), x, type = "categorical", values = values, x_arg = x_arg)
    )
    new_crunch_categorical_array_variable(
        x,
        label = label(to),
        description = description(to),
        notes = notes(to),
        path = path(to),
        values = values(to)
    )
}

#' @export
vec_cast.crunch_categorical_array_variable.tbl_df <- vec_cast.crunch_categorical_array_variable.data.frame
