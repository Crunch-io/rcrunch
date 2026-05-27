#' Create Crunch Categorical Variable
#'
#' Crunch categorical variables are kind of like R's base factors, but contain more
#' metadata. TODO: WRITE MORE!
#'
#' @param x A vector of values data
#' @param label The variable's label
#' @param values A data.frame of values metadata
#' @param description The variable's description
#' @param notes The variable's notes
#' @param path The variable's folder path
#' @param ... Ignored
#' @returns A `crunch_categorical_variable` object
#' @export
crunch_categorical_variable <- function(
        x,
        label,
        values,
        description = NULL,
        notes = NULL,
        path = NULL,
        ...
) {
    check_collect(
        "Invalid crunch_categorical_variable",
        check_single_string(label),
        check_single_string(description, null_ok = TRUE),
        check_single_string(notes, null_ok = TRUE),
        check_single_string(path, null_ok = TRUE),
        x <<- validate_category_value_data(x),
        values <<- validate_category_values(values, x)
    )

    new_crunch_categorical_variable(
        x = x,
        label = label,
        values = values,
        description = description,
        notes = notes,
        path = path
    )
}

new_crunch_categorical_variable <- function(
        x,
        label,
        values,
        description = NULL,
        notes = NULL,
        path = NULL,
        ...
) {
    out <- new_crunch_variable(
        x,
        label = label,
        description = description,
        notes = notes,
        path = path
    )

    # Augment to categorical
    attr(out, "values")  <- values
    class(out) <- c("crunch_categorical_variable", class(out))

    out
}

validate_category_value_data <- function(x, call = rlang::caller_env()) {
    if (inherits(x, c("crunch_categorical_variable", "crunch_categorical_array_variable"))) {
        return(x)
    }
    if (is.data.frame(x)) {
        orig_x <- x
        x <- vec_data(x)
        x[] <- lapply(x, validate_category_value_data, call = call)
        return(vec_restore(x, orig_x))
    }

    # Check if exactly numeric or integer class and convert to character if so
    if (identical(class(x), "numeric") || identical(class(x), "integer")) {
        # Convert metadata values to match data. This means that we have
        # string-ified integers, as we do in the parquet file. This seems
        # okay, since we don't really want to encourage "code" math where
        # people do math on the category codes
        x <- code_to_str(x)
    } else if (is.logical(x) && all(is.na(x))) {
        x <- as.character(x)
    }

    if (!inherits(x, "character")) {
        cli::cli_abort("category values must be {.cls character} or base {.cls numeric} types (got {.obj_type_friendly {x}})", call = call)
    }
    x
}

validate_category_values <- function(values, x, call = rlang::caller_env()) {
    check_collect(
        call = call,
        "Invalid values",
        check_inherits(values, "data.frame") |>
            check_if(
                check_df_has(values, "value") |>
                    check_if(
                        (values$value <<- validate_category_value_data(values$value)) |>
                        check_if(
                            check_unique(values$value),
                            check_nonmissing(values$value),
                            check_all_in(x, values$value, x_arg = "data")
                        )
                ),
                check_df_has(values, "label") |>
                    check_if(values$label <<- vec_cast(values$label, character(0))) |>
                    check_if(
                        check_unique(values$label),
                        check_nonmissing(values$label)
                    ),
                check_df_has(values, "missing") |>
                    check_if(values$missing <<- vec_cast(values$missing, logical(0))) |>
                    check_if(check_nonmissing(values$missing)),
                check_df_has(values, "scale") |>
                    check_if((values$scale <<- vec_cast(values$scale, numeric(0)))),
                check_df_has(values, "date") |>
                    check_if(values$date <<- vec_cast(values$date, character(0))) |>
                    check_if(
                        check_unique(values$date),
                        check_catdate_string(values$date)
                    )
            )
    )

    values
}

# ---- Attributes -----
#' @export
values.crunch_categorical_variable <- function(x, ...) {
    attr(x, "values", exact = TRUE)
}

#' @export
set_values.crunch_categorical_variable <- function(x, value, validate = TRUE, ...) {
    if (validate) {
        value <- validate_category_values(value, vec_data(x))
    }
    attr(x, "values") <- value
    x
}

# ---- Coercion -----
#' @export
as.factor.crunch_categorical_variable <- function(x, collapse_missing = TRUE, ...) {
    vals <- if (collapse_missing) non_missing_values(x) else values(x)
    factor(
        categorical_value_replaced_data(x, "label", collapse_missing),
        vals$label
    )
}

# TODO: document
#' @export
as.character.crunch_categorical_variable <- function(x, from = c("label", "value", "date"), collapse_missing = TRUE, ...) {
    from <- rlang::arg_match(from)
    categorical_value_replaced_data(x, from, collapse_missing)
}

# TODO: document
#' @export
as.double.crunch_categorical_variable <- function(x, from = c("value", "scale"), collapse_missing = TRUE, ...) {
    from <- rlang::arg_match(from)
    out <- categorical_value_replaced_data(x, from, collapse_missing)
    if (from == "value") {
        out <- values_to_numeric_or_fail(out)
    }
    out
}

#' @export
as.Date.crunch_categorical_variable <- function(x, ...) {
    date_strs <- categorical_value_replaced_data(x, "date", collapse_missing = TRUE)
    catdate_str_to_date(date_strs)
}

#' @export
as.POSIXct.crunch_categorical_variable <- function(x, ...) {
    as.POSIXct(as.Date(x), ...)
}

#' @export
as.POSIXlt.crunch_categorical_variable <- function(x, ...) {
    as.POSIXlt(as.Date(x), ...)
}

#' @export
as.logical.crunch_categorical_variable <- function(x, ...) {
    # Using backend's convention of having `NA` for selected when not selected
    # maybe should change on import
    vals <- values(x)
    vals$selected[is.na(vals$selected) & !vals$missing] <- FALSE
    x <- set_values(x, vals, validate = FALSE)
    categorical_value_replaced_data(x, "selected", collapse_missing = FALSE)
}


# ---- Type system -----
#' @importFrom methods setOldClass
setOldClass(c("crunch_categorical_variable", "crunch_variable", "vctrs_vctr"))

#' @export
#' @rdname crunch_categorical_variable
is.crunch_categorical_variable <- function(x) inherits(x, "crunch_categorical_variable")


#' @export
vec_ptype2.crunch_categorical_variable.crunch_categorical_variable <- function(
        x, y, ..., x_arg = rlang::caller_arg(x), y_arg = rlang::caller_arg(y), call = rlang::caller_env()
) {
    values <- coalesce_categorical_values(x, y, x_arg = x_arg, y_arg = y_arg, call = call, action = "combine")

    new_crunch_categorical_variable(
        character(0),
        label = label(x),
        values = values,
        description = description(x),
        notes = notes(x),
        path = path(x)
    )
}

#' @export
vec_ptype2.double.crunch_categorical_variable <- function(
        x, y, ..., x_arg = rlang::caller_arg(x), y_arg = rlang::caller_arg(y), call = rlang::caller_env()
) {
    values <- coalesce_categorical_values(x, y, x_arg = x_arg, y_arg = y_arg, call = call, action = "combine")

    new_crunch_categorical_variable(
        character(0),
        label = label(y),
        values = values,
        description = description(y),
        notes = notes(y),
        path = path(y)
    )
}

#' @export
vec_ptype2.integer.crunch_categorical_variable <- vec_ptype2.double.crunch_categorical_variable

#' @export
vec_ptype2.character.crunch_categorical_variable <- vec_ptype2.double.crunch_categorical_variable

#' @export
vec_ptype2.crunch_categorical_variable.integer <- function(x, y, ...) vec_ptype2(y, x, ...)

#' @export
vec_ptype2.crunch_categorical_variable.double <- vec_ptype2.crunch_categorical_variable.integer

#' @export
vec_ptype2.crunch_categorical_variable.character <- vec_ptype2.crunch_categorical_variable.integer


#' @export
vec_cast.crunch_categorical_variable.crunch_categorical_variable <- function(
        x, to, ..., x_arg = rlang::caller_arg(x), to_arg = rlang::caller_arg(to), call = rlang::caller_env()
) {
    values <- coalesce_categorical_values(x, to, x_arg = x_arg, y_arg = to_arg, call = call, action = "convert")

    new_crunch_categorical_variable(
        vctrs::vec_data(x),
        label = label(x),
        values = values,
        description = description(x),
        notes = notes(x),
        path = path(x)
    )

}

#' @export
vec_cast.double.crunch_categorical_variable <- function(x, to, ...) {
    data <- vctrs::vec_data(x)
    numeric <- values_to_numeric_or_fail(data)
    vctrs::vec_cast(numeric, to, ...)
}

#' @export
vec_cast.integer.crunch_categorical_variable <- vec_cast.double.crunch_categorical_variable

#' @export
vec_cast.character.crunch_categorical_variable <- function(x, to, ...) {
    vctrs::vec_cast(vctrs::vec_data(x), to, ...)
}

#' @export
vec_cast.crunch_categorical_variable.double <- function(
        x, to, ..., x_arg = rlang::caller_arg(x), to_arg = rlang::caller_arg(to), call = rlang::caller_env()
) {
    x <- code_to_str(x)
    values <- coalesce_categorical_values(x, to, x_arg = x_arg, y_arg = to_arg, call = call, action = "convert")

    new_crunch_categorical_variable(
        x,
        label = label(to),
        values = values,
        description = description(to),
        notes = notes(to),
        path = path(to)
    )
}

#' @export
vec_cast.crunch_categorical_variable.integer <- vec_cast.crunch_categorical_variable.double

#' @export
vec_cast.crunch_categorical_variable.character <- vec_cast.crunch_categorical_variable.double


# ---- Equality/Comparisons ----

#' @export
is.na.crunch_categorical_variable <- function(x) {
    is.na(vctrs::vec_data(x)) | categorical_value_replaced_data(x, "missing", collapse_missing = FALSE)
}

# Originally planned to use this so we wouldn't have to define `is.na()`
# However, when we do this, we get weird behavior for finding unique values
# like when we do `dplyr::group_by()` or `dplyr::distinct()` (it just grabs a random
# type of missing).
# We can make a method for `unique()`, but dplyr seems to use vctrs functions directly
# (including some that aren't generic)
# #' @export
# vec_proxy_equal.crunch_categorical_variable <- function(x, ...) {
#     vctrs::vec_cast(as.character(x, from = "value", collapse_missing = TRUE), x)
# }
#
# #' @export
# unique.crunch_categorical_variable <- function(x, incomparables = FALSE, ...) {
#     out <- NextMethod()
#     out[is.na(out)] <- NA # Replace with system missing
#     out
# }


#' @export
vec_proxy_compare.crunch_categorical_variable <- function(x, ...) {
    vctrs::stop_incompatible_op("compare", x, x, details = "Cannot compare categorical values")
}

#' @export
vec_proxy_order.crunch_categorical_variable <- function(x, ...) {
    # Collapse missings
    vals <- non_missing_values(x)

    match(vctrs::vec_data(x), vals$value)
}


# Don't want to do anything too fancy here, but don't want to allow comparing
# incomparable categories. vctrs warns against overriding functions like `==`, so
# just a quick check before calling the `vctrs` method. Also note that there's no
# double dispatch here, but luckily this situation could only happen when both
# sides are crunch variables so we don't have to
#' @export
`==.crunch_categorical_variable` <- function(e1, e2) {
    coalesce_categorical_values(e1, e2, x_arg = rlang::caller_arg(e1), y_arg = rlang::caller_arg(e2), call = rlang::caller_env(), action = "combine")
    NextMethod()
}

#' @export
`!=.crunch_categorical_variable` <- function(e1, e2) {
    coalesce_categorical_values(e1, e2, x_arg = rlang::caller_arg(e1), y_arg = rlang::caller_arg(e2), call = rlang::caller_env(), action = "combine")
    NextMethod()
}


# ---- Printing -----
# (Borrows heavily from haven; TODO: clean up)

# Dynamically exported, see zzz.R
pillar_shaft.crunch_categorical_variable <- function(x,
                                        show_labels = getOption("haven.show_pillar_labels", TRUE),
                                        ...) {
    if (!isTRUE(show_labels) | !pillar_print_pkgs_available()) {
        return(pillar::pillar_shaft(unclass(x)))
    }

    val <- val_num_pillar_info(x)
    lbl <- lbl_pillar_info(x)

    pillar::new_pillar_shaft(
        list(val = val, lbl = lbl),
        min_width = max(val$disp_short$lhs_ws + val$disp_short$main_wid + lbl$wid_short),
        width = max(val$disp_full$lhs_ws + val$disp_full$main_wid + lbl$wid_full),
        class = "pillar_shaft_crunch_categorical_variable"
    )
}

selection_pillar_info <- function(x) {
    no_selected <- !any(values(x)$selected)
    if (no_selected || is.na(no_selected)) return(list(text = rep("", length(x)), nchar = 0))

    text <- dplyr::if_else(
        as.logical(x),
        paste0(cli::symbol$tick, " "),
        paste0(pillar::style_neg(cli::symbol$cross), " "),
        ""
    )
    list(text = text, nchar = 2)
}


val_num_pillar_info <- function(x) {
    val_pillar <- pillar::pillar_shaft(as.character(x, from = "value", collapse_missing = FALSE))
    sel_pillar <- selection_pillar_info(x)
    disp_short <- num_disp_components(x, val_pillar, sel_pillar, min(nchar(x), 5, na.rm = TRUE))
    disp_full <- num_disp_components(x, val_pillar, sel_pillar, attr(val_pillar, "width"))

    list(
        disp_short = disp_short,
        disp_full = disp_full
    )
}

num_disp_components <- function(x, pillar, sel_pillar, width) {
    display <- format(pillar, width)
    # Sometimes there's an extra leading space from pillar
    display <- trim_ws_lhs(display)
    # Also add the selection tezt
    display <- paste0(sel_pillar$text, display)

    display_untrimmed_wid <- pillar::get_extent(display)
    display_max_wid <- max(display_untrimmed_wid)
    display <- trim_ws_rhs(display)
    main_wid <- pillar::get_extent(display)
    display_trimmed_rhs <- display_untrimmed_wid - main_wid

    display[is.na(unclass(x))] <- pillar::style_na(display[is.na(unclass(x))])
    list(
        lhs_ws = max(main_wid + display_trimmed_rhs) - (main_wid + display_trimmed_rhs),
        main_wid = main_wid,
        main_txt = display,
        rhs_ws = display_trimmed_rhs
    )
}

lbl_pillar_info <- function(x) {
    MIN_LBL_DISPLAY <- 6
    lbl <- as.character(x, from = "label", collapse_missing = FALSE)
    label_display <- ifelse(
        is.na(lbl),
        "",
        pillar::style_subtle(paste0(" [", utf8::utf8_encode(lbl), "]"))
    )
    label_widths <- pillar::get_extent(label_display)
    label_min_widths <- ifelse(label_widths > 0, pmin(MIN_LBL_DISPLAY, label_widths), 0)

    MIN_NA_DISPLAY <- 4
    na_display <- character(length(x))
    missing_values <- categorical_value_replaced_data(x, "missing", collapse_missing = FALSE)
    na_display[missing_values & !is.na(missing_values)] <- pillar::style_na(" (NA)")
    na_widths <- pillar::get_extent(na_display)

    label_display <- paste0(na_display, label_display)
    label_widths <- label_widths + na_widths
    label_min_widths <- label_min_widths + ifelse(label_widths > 0, pmin(MIN_NA_DISPLAY, label_widths), 0)

    ret <- list(
        wid_short = label_min_widths,
        disp_full = label_display,
        wid_full = label_widths
    )
    ret
}

#' @export
format.pillar_shaft_crunch_categorical_variable <- function(x, width, ...) {
    vshort <- x$val$disp_short
    vfull <- x$val$disp_full
    lbl_wid <- pmax(0, x$lbl$wid_short - vfull$rhs_ws)

    if (width >= max(vfull$lhs_ws + vfull$main_wid + lbl_wid)) {
        lbl_width <- width - (vfull$lhs_ws + vfull$main_wid)
        lbl <- str_trunc(x$lbl$disp_full, lbl_width, subtle = TRUE)
        out <- paste_with_align(vfull$main_txt, lbl, vfull$lhs_ws, vfull$rhs_ws)
    } else {
        lbl_width <- width - (vshort$lhs_ws + vshort$main_wid)
        lbl <- str_trunc(x$lbl$disp_full, lbl_width, subtle = TRUE)
        out <- paste_with_align(vshort$main_txt, lbl, vshort$lhs_ws, vshort$rhs_ws)
    }
    pillar::new_ornament(out, width = width, align = "left")
}



# TODO
# #' @export
# summary.crunch_categorical_variable <- function(object, maxsum = 100, collapse_missing = TRUE, ...) {
#     # tbl <- table(vctrs::vec_data(object), useNA = "always")
# }


# ---- Formatting ----

#' @export
vec_ptype_abbr.crunch_categorical_variable <- function(x, ...) {
    "cr_cat"
}

#' @export
format.crunch_categorical_variable <- function(x, ...) {
    # TODO: Make better?
    format(vctrs::vec_data(x), ...)
}

#' @export
obj_print_footer.crunch_categorical_variable <- function(x, ...) {
    # TODO: other attributes?

    cat("\nCategories:", "\n", sep = "")
    print.data.frame(values(x), row.names = FALSE, max = 60) # 10 rows max

    invisible(x)
}


# ---- Arithmetic ----
# Intentionally not allowed
# Doing math on codes doesn't need to be easy




# ---- Helpers -----
categorical_value_replaced_data <- function(x, type, collapse_missing = TRUE) {
    vals <- values(x)
    rlang::arg_match(type, names(vals))

    if (collapse_missing) vals[vals$missing, ][[type]] <- NA
    vals[[type]][match(vctrs::vec_data(x), vals$value)]
}

coalesce_categorical_values <- function(x, y, x_arg, y_arg, call, action) {
    # For now, always have strategy = "exact"
    # TODO: strategy needs to be configurable without passing in the argument some how
    #   probably via an env var or something. This function isn't directly called
    #   but users may want to set their defaut strategy
    # Some ideas for strategy:
    # c("expand_well_defined", "expand_all", "exact", "match_values", "match_labels")

    if (is.crunch_categorical_variable(x) & is.crunch_categorical_variable(y)) {
        x_values <- values(x)
        y_values <- values(y)

        if (identical(x_values, y_values)) return(x_values)
        error_details <- "Category values do not match"

        exact_matches <- dplyr::inner_join(x_values, y_values, by = names(x_values))
        if (nrow(exact_matches) > 0) {
            error_details <- c(error_details, v = cli::format_inline("{nrow(exact_matches)} match exactly"))
        }

        x_only_values <- dplyr::anti_join(x_values, y_values, by = names(x_values))
        y_only_values <- dplyr::anti_join(y_values, x_values, by = names(x_values))
        if (nrow(x_only_values) == 0 && nrow(y_only_values) == 0) {
            # TODO: summarize order differences
            error_details <- c(error_details, x = cli::format_inline("Category ordering is different"))
        } else if (nrow(x_only_values) == 0 || nrow(y_only_values) == 0 ) {
            if (nrow(x_only_values) == 0) {
                new_values <- y_only_values
                new_arg <- y_arg
            } else {
                new_values <- x_only_values
                new_arg <- x_arg
            }
            new_ids <- paste0(new_values$value, " - ",  new_values$label)
            error_details <- c(
                error_details,
                x = cli::format_inline("{nrow(new_values)} categeories are only in `{new_arg}` ({truncated_list(new_ids)})")
            )
        } else {
            value_label_matches <- dplyr::inner_join(x_only_values, y_only_values, by = c("value", "label"))
            if (nrow(value_label_matches) > 0) {
                value_label_matches <- dplyr::mutate(
                    value_label_matches,
                    missing_diff = !purrr::map2_lgl(.data$missing.x, .data$missing.y, identical),
                    scale_diff = !purrr::map2_lgl(.data$scale.x, .data$scale.y, identical),
                    selected_diff = !purrr::map2_lgl(.data$selected.x, .data$selected.y, identical),
                    date_diff = !purrr::map2_lgl(.data$date.x, .data$date.y, identical)
                )

                differ_types <- dplyr::summarize(value_label_matches, dplyr::across(dplyr::ends_with("diff"), any)) |>
                    as.list() |>
                    purrr::keep(identity) |>
                    names() |>
                    gsub(pattern = "_diff$", replacement = "")


                differ_info <- paste0(
                    value_label_matches$value, " - ", value_label_matches$label, ":",
                    ifelse(value_label_matches$missing_diff, paste0(" missing= ", value_label_matches$missing.x, " vs ", value_label_matches$missing.y), ""),
                    ifelse(value_label_matches$scale_diff, paste0(" scale= ", value_label_matches$scale.x, " vs ", value_label_matches$scale.y), ""),
                    ifelse(value_label_matches$selected_diff, paste0(" selected= ", value_label_matches$selected.x, " vs ", value_label_matches$selected.y), ""),
                    ifelse(value_label_matches$date_diff, paste0(" date= ", value_label_matches$date.x, " vs ", value_label_matches$date.y), "")
                )

                error_details <- c(
                    error_details,
                    x = cli::format_inline("{nrow(value_label_matches)} match on value and label, but not on {.or {differ_types}} ({truncated_list(differ_info)})")
                )
            }

            label_matches <- dplyr::inner_join(x_only_values, y_only_values, by = setdiff(names(x_only_values), "value"))
            if (nrow(label_matches) > 0) {
                differ_info <- paste0(label_matches$label, ": value= ", label_matches$value.x, " vs ", label_matches$value.y)

                error_details <- c(
                    error_details,
                    x = cli::format_inline("{nrow(label_matches)} match on everything but value ({truncated_list(differ_info)})")
                )
            }

            value_matches <- dplyr::inner_join(x_only_values, y_only_values, by = setdiff(names(x_only_values), "label"))
            if (nrow(value_matches) > 0) {
                differ_info <- paste0(value_matches$value, ": label= ", value_matches$label.x, " vs ", value_matches$label.y)

                error_details <- c(
                    error_details,
                    x = cli::format_inline("{nrow(value_matches)} match on everything but label ({truncated_list(differ_info)})")
                )
            }

            no_x_matches <- x_only_values |>
                dplyr::filter(
                    !.data$value %in% value_label_matches$value &
                    !.data$label %in% label_matches$label &
                    !.data$value %in% value_matches$value
                )

            if (nrow(no_x_matches) > 0) {
                differ_info <- paste0(no_x_matches$value, " - ", no_x_matches$label)
                error_details <- c(
                    error_details,
                    x = cli::format_inline("{nrow(no_x_matches)} in `{x_arg}` have no match ({truncated_list(differ_info)})")
                )
            }
            no_y_matches <- y_only_values |>
                dplyr::filter(
                    !.data$value %in% value_label_matches$value &
                        !.data$label %in% label_matches$label &
                        !.data$value %in% value_matches$value
                )

            if (nrow(no_y_matches) > 0) {
                differ_info <- paste0(no_y_matches$value, " - ", no_y_matches$label)
                error_details <- c(
                    error_details,
                    x = cli::format_inline("{nrow(no_y_matches)} in `{y_arg}` have no match ({truncated_list(differ_info)})")
                )
            }
        }

        vctrs::stop_incompatible_type(
            x, y,
            action = action,
            details = error_details,
            x_arg = x_arg,
            y_arg = y_arg,
            call = call
        )

    } else {
        if (is.crunch_categorical_variable(x)) {
            values <- values(x)
            uniques <- unique(y)
        } else {
            values <- values(y)
            uniques <- unique(x)
        }
        uniques <- uniques[!is.na(uniques)]
        extra_values <- setdiff(uniques, values$value)

        valid <- length(extra_values) == 0
        details <- paste0("Values not found in categories: ", truncated_list(extra_values))
    }

    if (!valid) {
        vctrs::stop_incompatible_type(
            x, y,
            action = action,
            details = details,
            x_arg = x_arg,
            y_arg = y_arg,
            call = call
        )
    }
    values
}

values_to_numeric_or_fail <- function(x) {
    numeric <- suppressWarnings(as.numeric(x))
    if (sum(is.na(x)) != sum(is.na(x))) vctrs::stop_incompatible_cast(x, numeric(), message = "Categorical data stored as non-numeric codes")
    numeric
}

non_missing_values <- function(x) {
    values <- values(x)
    values[!values$missing, ]
}

# Codes are categorical values, which are only ever integers
code_to_str <- function(x) {
    if (is.character(x)) return(x)
    out <- sprintf(vec_cast(x, integer(0), x_arg = "category code"), fmt = "%0.f")
    out[is.na(x)] <- NA
    out
}


# --- Cat Date Helpers ----
CATDATE_REGEXES <- list(
    year = "^[0-9]{4}$",
    month = "^[0-9]{4}-[0-9]{1,2}$",
    day = "^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}$",
    week = "^[0-9]{4}-W[0-9]{1,2}$"
)


catdate_str_to_date <- function(x) {
    out <- as.Date(rep(NA, length(x)))

    year_formatted_idx <- grepl(CATDATE_REGEXES$year, x)
    out[year_formatted_idx]  <- as.Date(paste0(x[year_formatted_idx], "-01-01"), format = "%Y-%m-%d")
    month_formatted_idx <- grepl(CATDATE_REGEXES$month, x)
    out[month_formatted_idx] <- as.Date(paste0(x[month_formatted_idx], "-01"), format = "%Y-%m-%d")
    day_formatted_idx <- grepl(CATDATE_REGEXES$day, x)
    out[day_formatted_idx] <- as.Date(x[day_formatted_idx], format = "%Y-%m-%d")
    week_formatted_idx <- grepl(CATDATE_REGEXES$week, x)
    out[week_formatted_idx] <- as.Date(paste0(x[week_formatted_idx], "-1"), format = "%Y-W%W-%u")

    if (any(is.na(out) & !is.na(x))) {
        cli::cli_abort(
            "Unexpected categorical date formats found: {truncated_list(x[is.na(out) & !is.na(x)])}"
        )
    }
    out

}

check_catdate_string <- function(x, x_arg = rlang::caller_arg(x), call = rlang::caller_env()) {
    if (all(is.na(x))) return(x) # Fast track common case
    year_formatted_idx <- grepl(CATDATE_REGEXES$year, x)
    month_formatted_idx <- grepl(CATDATE_REGEXES$month, x)
    day_formatted_idx <- grepl(CATDATE_REGEXES$day, x)
    week_formatted_idx <- grepl(CATDATE_REGEXES$week, x)
    na_idx <- is.na(x)
    good_idx <- year_formatted_idx | month_formatted_idx | day_formatted_idx | week_formatted_idx | na_idx

    if (!all(good_idx)) {
        bad <- truncated_list(x[!good_idx])
        cli::cli_abort(
            "{.arg {x_arg}} contains incorrectly formatted date strings ({bad})",
            x_arg = x_arg, bad = bad, call = call
        )
    }

    # TODO: Replicate other backend validation, eg:
    # - Not allowed to have any nonmissing values without date if any has a date
    # - Dates must be increasing in order
    # - Cannot mix week with other types of datestrings
    # - More?
    invisible(x)
}

#' @export
mean.crunch_categorical_variable <- function(x, ..., from = c("scale", "selected")) {
    from <- rlang::arg_match(from)
    if (from == "selected") {
        # Using backend's convention of having `NA` for selected when not selected
        # maybe should change on import
        vals <- values(x)
        vals$selected[is.na(vals$selected) & !vals$missing] <- FALSE
        x <- set_values(x, vals, validate = FALSE)
    }

    mean(categorical_value_replaced_data(x, from), ...)
}

