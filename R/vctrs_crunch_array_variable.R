#' @export
is.crunch_array_variable <- function(x) {
    inherits(x, "crunch_array_variable")
}

#' @export
axes.crunch_array_variable <- function(x, ...) {
    out <- list(purrr::imap_dfr(x, ~tibble::tibble(
        value = .y,
        label = label(.x),
        description = description(.x) %||% NA_character_
    )))

    if (is.crunch_array_variable(x[[1]])) {
        out[[2]] <- dplyr::bind_rows(
            purrr::imap_dfr(x[[1]], ~tibble::tibble(
                value = .y,
                label = label(.x),
                description = description(.x) %||% NA_character_
            ))
        )
    }
    out
}


validate_axes <- function(axes, x, type, ..., call = rlang::caller_env()) {
    if (is.null(axes)) {
        if (type == "categorical") {
            allowed_types <- c("crunch_categorical_variable", "crunch_categorical_array_variable") }
        else  {
            allowed_types <- c("crunch_numeric_variable", "crunch_numeric_array_variable")
        }
        all_var_types <- purrr::map(x, class)

        check_collect(
            "Invalid inferred axis structure",
            check_true(
                all(purrr::map_lgl(all_var_types, ~any(. %in% allowed_types))),
                "All columns must be crunch variables of type {.or {allowed_types}}"
            ) |>
                check_if(check_true(
                    length(unique(all_var_types)) == 1,
                    "All columns must be of the same type"
                ))
        )
        # Gather axes
        axes <- purrr::imap_dfr(x, ~tibble::tibble(
            value = .y,
            label = label(.x),
            description = description(.x) %||% NA_character_,
            sub_axes = if (is.crunch_array_variable(.x)) list(axes(.x)) else NA
        ))

        check_collect(
            "Invalid inferred axis structure",
            check_unique(axes$label, arg = "axis item labels", call = call),
            check_true(
                length(unique(axes$sub_axes)) == 1,
                "Second dimension of component arrays must exactly match"
            ) |>
                check_if(check_true(
                    all(is.na(axes$sub_axes)) || length(axes$sub_axes[[1]]) == 1,
                    "Arrays cannot have 3rd dimension"
                ))
        )
        # Remove attributes that don't apply to subvariables
        orig_x <- x
        x <- vec_data(x)
        x[] <- lapply(x, function(col) {
            old_attributes <- attributes(col)
            attributes(col) <- old_attributes[setdiff(names(old_attributes), c("notes", "path"))]
            col
        })
        x <- vec_restore(x, orig_x)
    } else {
        check_collect(
            "Invalid axes",
            check_inherits(axes, "list") |>
                check_if(
                    check_true(all(purrr::map_lgl(axes, is.data.frame)), "axes must be a list of data.frames"),
                    check_true(length(axes) %in% 1:2, "axes can only be 1 or 2 dimensional")
                ) |> check_if(
                    axes[[1]] <<- check_axis_dim(axes[[1]], 1, x),
                    if (length(axes) > 1) axes[[2]] <<- check_axis_dim(axes[[2]], 2, x)
                )
        )

        # Add axis metadata onto array items
        # TODO: Is this okay as true categorical variables or should we have an array item class?
        # see vctrs_crunch_array_item.R
        if (type == "categorical" && length(axes) == 1) {
            cv_func <- new_crunch_categorical_variable
        } else if (type == "numeric" && length(axes) == 1) {
            cv_func <- crunch_numeric_variable
        } else if (type == "categorical") {
            cv_func <- new_crunch_categorical_array_variable
            inner_cv_func <- new_crunch_categorical_variable
        } else {
            cv_func <- crunch_numeric_array_variable
            inner_cv_func <- crunch_numeric_variable
        }

        outer_metadata <- axes[[1]]
        if (length(axes) > 1) {
            inner_metadata <- axes[[2]]
            lapply(outer_metadata$value, function(outer_name) {
                x[[outer_name]][] <<- lapply(inner_metadata$value, function(inner_name) {
                    array_item_metadata <- inner_metadata[inner_metadata$value == inner_name, ]
                    inner_cv_func(
                        x[[outer_name]][[inner_name]],
                        label = array_item_metadata$label,
                        description = if (!is.na(array_item_metadata$description)) array_item_metadata$description,
                        ...
                    )
                })
            })
        }

        orig_x <- x
        x <- vec_data(x)
        x[] <- lapply(outer_metadata$value, function(item_name) {
            array_item_metadata <- outer_metadata[outer_metadata$value == item_name, ]
            cv_func(
                x[[item_name]],
                label = array_item_metadata$label,
                description = if (!is.na(array_item_metadata$description)) array_item_metadata$description,
                axes = NULL,
                ...
            )
        })
        x <- vec_restore(x, orig_x)
    }
    x
}

check_axis_dim <- function(axis, dim_num, x) {
    check_collect(
        paste0("axes[[", dim_num, "]]"),
        check_df_has(axis, "value") |>
            check_if(
                axis$value <<- vec_cast(axis$value, character(0)),
                check_unique(axis$value)
            ) |>
            check_if({
                if (dim_num == 1) {
                    check_collect(
                        message = NULL,
                        check_true(
                            identical(axis$value, names(x)),
                            "Axis values must match column names"
                        )
                    )
                } else {
                    check_collect(
                        message = NULL,
                        check_true(
                            all(purrr::map_lgl(x, ~is.data.frame(.))),
                            "Columns must all be data.frames for 2D array"
                        ) |> check_if(
                            check_true(
                                all(purrr::map_lgl(x, ~identical(names(.), axis$value))),
                                "Axis values in 2nd dimension must match inner column names"
                            )
                        )
                    )
                }
            }),
        check_df_has(axis, "label") |>
            check_if(
                axis$label <<- vec_cast(axis$label, character(0)),
                check_unique(axis$label)
            ),
        check_df_has(axis, "description") |>
            check_if(axis$description <<- vec_cast(axis$description, character(0)))
    )
    axis
}

#' @export
as.factor.crunch_array_variable <- function(x, ...) {
    lapply(x, function(x) as.factor(x, ...)) |>
        tibble::as_tibble()
}

#' @export
as.character.crunch_array_variable <- function(x, ...) {
    lapply(x, function(x) as.character(x, ...)) |>
        tibble::as_tibble()
}

#' @export
as.double.crunch_array_variable <- function(x, ...) {
    lapply(x, function(x) as.double(x, ...)) |>
        tibble::as_tibble()
}

#' @export
as.logical.crunch_array_variable <- function(x, ...) {
    lapply(x, function(x) as.logical(x, ...)) |>
        tibble::as_tibble()
}

#' @export
as.data.frame.crunch_array_variable <- function(x, ...) {
    out <- NextMethod()
    old_attributes <- attributes(out)
    attributes(out) <- attributes(out)[setdiff(names(old_attributes), c("label", "description", "notes", "path", "values"))]
    out
}

#' @importFrom tibble as_tibble
#' @export
as_tibble.crunch_array_variable <- function(x, ...) {
    out <- NextMethod()
    old_attributes <- attributes(out)
    attributes(out) <- attributes(out)[setdiff(names(old_attributes), c("label", "description", "notes", "path", "values"))]
    out
}

#' @export
`names<-.crunch_array_variable` <- function(x, value) {
    if (is.null(value)) {
        cli::cli_abort("`crunch_array_variable`s must have names")
    }
    if (length(value) != length(x)) {
        cli::cli_abort(
            "`crunch_array_variable`s must have names for each element (got names of length {length(value)} for {length(x)} values)"
        )
    }

    check_unique(value)
    NextMethod()
}

# ---- Subsetting/ Sub assigning ----
#' @export
`[.crunch_array_variable` <- function(x, i, j, ...) {
    i_missing <- missing(i)
    j_missing <- missing(j)
    de_facto_j <- if (j_missing & !i_missing) i else if (!j_missing) j else NULL
    if (missing(de_facto_j) || !is.list(de_facto_j)) {
        return(NextMethod())
    }

    # if j is list, use it to allow subsetting across 2D vectors
    num_axes <- length(axes(x))
    len_j <- length(de_facto_j)
    check_true(
        all(purrr::map_lgl(de_facto_j, ~inherits(., c("NULL", "logical", "integer", "numeric", "character")))),
        "array list subscript items can only be logical, numeric, character or NULL in `[`"
    )
    check_true(
        num_axes == len_j,
        "array list subscript must have the same length as number of axes ({num_axes}, but got {len_j})"
    )

    if (all(purrr::map_lgl(de_facto_j, is.null))) {
        if (!i_missing && !j_missing) return(x[i, ])
        else return(x[])
    }

    # If second dimension selection is NULL (or if we have 1D with a list),
    # we can behave like it's just the first item i/j
    if (length(de_facto_j) == 1 || is.null(de_facto_j[[2]])) {
        if (j_missing) i <- de_facto_j[[1]] else j <- de_facto_j[[1]]
        return(NextMethod())
    }

    if (all(purrr::map_lgl(de_facto_j, is.null))) {
        if (!i_missing && !j_missing) {
            return(x[i, ])
        } else {
            return(x[])
        }
    }

    if (!is.null(de_facto_j[[1]])) {
        if (j_missing) i <- de_facto_j[[1]] else j <- de_facto_j[[1]]
        next_method_out <- NextMethod()
        # If first dimension is NULL, then we have to iterate across 2nd dimension
        out <- vec_data(next_method_out)
        out[] <- lapply(seq_along(out), function(outer_idx) {
            inner <- out[[outer_idx]]
            if (!i_missing && !j_missing) return(inner[i, de_facto_j[[2]]])
            inner[de_facto_j[[2]]]
        })
        out <- vec_restore(out, next_method_out)
        return(out)
    }

    if (!i_missing && !j_missing) {
        initial_out <- x[i, ]
    } else {
        initial_out <- x
    }
    out <- vec_data(initial_out)
    out[] <- lapply(seq_along(out), function(item_idx) {
        out[[item_idx]][de_facto_j[[2]]]
    })
    vec_restore(out, initial_out)
}

#' @export
`[<-.crunch_array_variable` <- function(x, i, j, ..., value) {
    i_missing <- missing(i)
    j_missing <- missing(j)
    de_facto_j <- if (j_missing & !i_missing) i else if (!j_missing) j else NULL
    if (is.list(de_facto_j)) {
        stop("TODO: list subscripting on arrays with `[<-` not yet supported")
    }
    NextMethod()

}

#' @export
`[[.crunch_array_variable` <- function(x, i, j, ...) {
    i_missing <- missing(i)
    j_missing <- missing(j)
    de_facto_j <- if (j_missing & !i_missing) i else if (!j_missing) j else NULL
    if (missing(de_facto_j) || !is.list(de_facto_j)) {
        return(NextMethod())
    }

    # if j is list, use it to allow subsetting across 2D vectors
    num_axes <- length(axes(x))
    len_j <- length(de_facto_j)
    check_true(
        all(purrr::map_lgl(de_facto_j, ~inherits(., c("NULL", "logical", "integer", "numeric", "character")))),
        "array list subscript items can only be logical, numeric, character or NULL in `[[`"
    )
    check_true(
        num_axes == len_j,
        "array list subscript must have the same length as number of axes ({num_axes}, but got {len_j})"
    )
    check_true(
        !all(purrr::map_lgl(de_facto_j, is.null)),
        "array subscript list can't be NULL for all dimensions in `[[`"
    )
    check_true(
        all(lengths(de_facto_j) %in% c(0, 1)),
        "array list subscript items can only be length 1 or 0 for each dimension in `[[`"
    )

    # If second dimension selection is NULL (or if we have 1D with a list),
    # we can behave like it's just the first item i/j
    if (length(de_facto_j) == 1 || is.null(de_facto_j[[2]])) {
        if (missing(j)) i <- de_facto_j[[1]] else j <- de_facto_j[[1]]
        return(NextMethod())
    }

    if (!is.null(de_facto_j[[1]])) {
        if (missing(j)) i <- de_facto_j[[1]] else j <- de_facto_j[[1]]
        out <- NextMethod()
        return(out[[de_facto_j[[2]]]])
    }

    if (!missing(i) && !missing(j)) {
        out <- x[i, ]
    } else {
        out <- x
    }

    if (inherits(x, "crunch_categorical_array_variable")) {
        inner_f <- function(x, ...) {
            new_crunch_categorical_variable(x, ..., values = values(out))
        }
        outer_f <-  function(x, ...) {
            new_crunch_categorical_array_variable(x, ..., values = values(out))
        }
    } else {
        inner_f <- new_crunch_numeric_variable
        outer_f <- new_crunch_numeric_array_variable
    }
    data <- lapply(seq_along(out), function(item_idx) {
        inner_data <- out[[item_idx]][[de_facto_j[[2]]]]
        # If doesn't exist, return NULL
        if (is.null(inner_data)) return(NULL)
        inner_f(
            inner_data,
            label = label(out[[item_idx]]),
            description = description(out[[item_idx]])
        )
    })

    # Couldn't find inner data, so return NULL
    if (is.null(data[[1]])) return(NULL)

    outer_f(
        data |> setNames(names(out)),
        label = label(out[[1]][[de_facto_j[[2]]]]),
        description = description(out[[1]][[de_facto_j[[2]]]])
    )

}

#' @export
`[[<-.crunch_array_variable` <- function(x, i, j, ..., value) {
    value_arg <- rlang::caller_arg(value)
    i_missing <- missing(i)
    j_missing <- missing(j)
    if (i_missing && j_missing) {
        return(NextMethod())
    }
    de_facto_j <- if (j_missing & !i_missing) i else if (!j_missing) j else NULL

    if (is.list(de_facto_j)) {
        # if j is list, use it to allow subsetting across 2D vectors
        num_axes <- length(axes(x))
        len_j <- length(de_facto_j)
        check_true(
            all(purrr::map_lgl(de_facto_j, ~inherits(., c("NULL", "logical", "integer", "numeric", "character")))),
            "array list subscript items can only be logical, numeric, character or NULL in `[[`"
        )
        check_true(
            num_axes == len_j,
            "array list subscript must have the same length as number of axes ({num_axes}, but got {len_j})"
        )
        check_true(
            !all(purrr::map_lgl(de_facto_j, is.null)),
            "array subscript list can't be NULL for all dimensions in `[[`"
        )
        check_true(
            all(lengths(de_facto_j) %in% c(0, 1)),
            "array list subscript items can only be length 1 or 0 for each dimension in `[[`"
        )
    }
    lbl <- default_array_assign_label(value, de_facto_j)

    template_attempt <- try(x[[de_facto_j]], silent = TRUE)
    if (!is.null(template_attempt) && !inherits(template_attempt, "try-error")) {
        # Modifying existing data
        template <- template_attempt
    } else if (!is.list(de_facto_j) || length(de_facto_j) == 1 || is.null(de_facto_j[[2]])) {
        # Modifying along outer dimension
        if (ncol(x) > 0) {
            template <- x[[1]] |> set_label(lbl) |> set_description(NULL)
        } else {
            stop("TODO: Add to empty crunch array")
        }
    } else if (is.null(de_facto_j[[1]])) {
        # Modifying along outer dimension
        if (ncol(x) > 0 && ncol(x[[1]]) > 0) {
            template <- x[[list(NULL, 1)]] |> set_label(lbl) |> set_description(NULL)
        } else {
            stop("TODO: Add to empty crunch array")
        }
    } else {
        # Modifying along outer dimension
        if (ncol(x) > 0 && ncol(x[[1]]) > 0) {
            template <- x[[list(1, 1)]] |> set_label(lbl) |> set_description(NULL)
        } else {
            stop("TODO: Add to empty crunch array")
        }
    }
    # If we're replacing in i, don't want to bring in metadata from replacement
    if (!i_missing && !j_missing && (!is.null(template_attempt) && !inherits(template_attempt, "try-error")) && !is.null(value)) {
        value <- vec_data(value)
    }

    value <- vec_cast(value, template, x_arg = value_arg)

    # If not a list then can use NextMethod
    if (!is.list(de_facto_j)) {
        out <- NextMethod()
    } else if (length(de_facto_j) == 1 || is.null(de_facto_j[[2]])) {
        # If second dimension selection is NULL (or if we have 1D with a list),
        # we can behave like it's just the first item i/j
        # Can't use NextMethod() because it causes confusion about i/j & missingness
        out <- x
        if (i_missing || j_missing) {
            out[[de_facto_j[[1]]]] <- value
        } else {
            out[[i, de_facto_j[[1]]]] <- value
        }
    } else if (is.null(de_facto_j[[1]])) {
        # If first dimension is NULL, then we have to iterate across 2nd dimension
        out <- vec_data(x)
        out[] <- lapply(seq_along(out), function(outer_idx) {
            inner <- out[[outer_idx]]
            replacement <- value[[outer_idx]]
            if (!is.null(replacement)) {
                replacement <- replacement |>
                    set_label(label(value)) |>
                    set_description(description(value))
            }
            if (!i_missing && !j_missing) inner[[i, de_facto_j[[2]]]] <- replacement
            else inner[[de_facto_j[[2]]]] <- replacement
            inner
        })
        out <- vec_restore(out, x)
    } else {
        out <- x
        if (!i_missing && !j_missing) out[[i, de_facto_j[[1]]]][[de_facto_j[[2]]]] <- value
        else out[[de_facto_j[[1]]]][[de_facto_j[[2]]]] <- value
    }
    # If we added/removed then we may have invalidated the axes, so check again
    type <- if (inherits(x, c("crunch_categorical_array_variable"))) "categorical" else "numeric"
    validate_axes(NULL, out, type)
    return(out)
}

#' @export
`$<-.crunch_array_variable` <- function(x, name, value) {
    x[[name]] <- value
    x
}


#' Apply function to each column of a packed data.frame array column
#'
#' @param x A data.frame
#' @param func A function to apply to each column
#' @param ... Arguments passed to `func`
#'
#' @export
array_apply <- function(x, func, ...) {
    func <- purrr::as_mapper(func)
    tibble::as_tibble(lapply(x, function(sub) func(sub, ...)))
}

#' @export
mean.crunch_array_variable <- function(x, ...) {
    array_apply(x, mean, ...)
}

#' @importFrom stats median
#' @export
median.crunch_array_variable <- function(x, ...) {
    array_apply(x, median, ...)
}

#' @importFrom stats quantile
#' @export
quantile.crunch_array_variable <- function(x, ...) {
    array_apply(x, quantile, ...)
}


# dynamic export; see zzz.R
dplyr_col_modify.crunch_array_variable <- function(data, cols) {
    call <- rlang::caller_env()
    cast_target <- determine_array_item_ptype(cols, orig = data)
    cols[] <- lapply(names(cols), function(col_name) {
        value <- cols[[col_name]]
        if (!is.crunch_variable(value) && !is.null(value)) {
            value <- vec_cast(value, cast_target |> set_label(col_name), x_arg = col_name)
        }
        value
    })
    NextMethod()
}

determine_array_item_ptype <- function(columns, orig) {
    # If all NULL, return NULL (removing columns)
    columns <- purrr::discard(columns, is.null)
    if (length(columns) == 0) return(NULL)

    # If all NULL/1D, ptype is just an categorical/numeric
    if (!any(purrr::map_lgl(columns, is.data.frame))) {
        if (is.crunch_categorical_array_variable(orig)) {
            return(new_crunch_categorical_variable(character(0), label = "???", values = values(orig)))
        } else {
            return(new_crunch_numeric_variable(character(0), label = "???"))
        }
    }

    # If any of the new columns are crunch array variables, use the first one
    array_columns <- purrr::keep(columns, is.crunch_array_variable)
    if (length(array_columns) > 0) return(array_columns[[1]])

    # If they match the dimensions of the original columns, then use the original
    if (ncol(columns[[1]]) == ncol(orig[[1]])) {
        return(orig[[1]])
    }

    # If nothing else, use the variable names as labels
    axes <- list(purrr::map_dfr(names(columns[[1]]), ~data.frame(value = ., label = ., description = NA)))
    if (is.crunch_categorical_array_variable(orig)) {
        return(crunch_categorical_array_variable(
            as_tibble(lapply(names(columns[[1]]), character(0))),
            label = "???",
            values = values(orig),
            axes = axes))
    } else {
        return(crunch_numeric_array_variable(
            as_tibble(lapply(names(columns[[1]]), numeric(0))),
            label = "???",
            axes = axes
        ))
    }
}

default_array_assign_label <- function(x, de_facto_j) {
    if (is.crunch_variable(x)) return(label(x))
    if (is.character(de_facto_j)) return(de_facto_j)
    non_null_j <- purrr::discard(de_facto_j, is.null)
    out <- non_null_j[[length(non_null_j)]]
    # TODO: if non-character use tidyverse name repair somehow?
    if (!is.character(out)) out <- rlang::as_label(out)
    out
}


# ---- Exploration of having crunch items instead of base crunch variables

# Not actually used
# new_crunch_array_item <- function(x, label, description = NULL) {
#     vctrs::new_vctr(
#         x,
#         label = label,
#         description = description,
#         class = "crunch_array_item",
#         inherit_base_type = TRUE
#     )
# }

# If we did this, we'd want subsetting to array variables to work like this
# #' @export
# `[[.crunch_array_variable` <- function(x, i, j, ...) {
#     # TODO: if i is list, use it to allow subsetting across 2D vectors
#     vdata <- vctrs::vec_data(x)[[i, j, ...]]
#     new_crunch_categorical_variable(
#         vdata,
#         label = attr(vdata, "label"),
#         description = attr(vdata, "description"),
#         values = attr(x, "values")
#     )
# }
