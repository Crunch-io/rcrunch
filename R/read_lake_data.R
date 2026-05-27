#' Read downloaded Crunch Lake data files into a 'tidy' format
#'
#' Work-in-progress R representation of the lake data. See [`cr_read_data_long()`] to
#' read the data in the format it is sent.
#'
#' @param x A folder path or destination from [`cr_export()`]
#' @param as_crunch_variables Whether to load the data as special data structures with
#' crunch metadata (defaults to `TRUE`)
#' @param array_strategy One of "packed" for packed data.frame columns to represent
#' array variables, "qualified" to have flattened representations of the array variables
#' with the parent array's alias before the axis item names to distinguish items with the
#' same axis value but from another array, or "unqualified" which only uses the array
#' axis item.
#' @param name_repair Passed to [`vctrs::vec_as_names()`] to fix column name collisions
#' that can arise due to array variables. Defaults to "check_unique", which gives an error
#'  when finding duplicate names.
#' @param ... Saved for future expansion
#'
#' @export
cr_read_data <- function(
        x,
        as_crunch_variables = TRUE,
        array_strategy = c("packed", "qualified", "unqualified"),
        name_repair = "check_unique",
        ...
) {
    array_strategy <- rlang::arg_match(array_strategy)
    if (crunch::is.dataset(x)) x <- download_lake_data_to_temp(x)
    data <- cr_read_data_long(x, ...)
    combined_metadata <- cr_read_meta(x, ...)

    combined_metadata <- purrr::map_dfr(
        seq_len(nrow(combined_metadata)),
        ~array_lookup_info(combined_metadata[., ], array_strategy)
    )

    combined_metadata$name <- vctrs::vec_as_names(combined_metadata$name, repair = name_repair)

    if (array_strategy == "packed") {
        if (all(lengths(combined_metadata$axis_values) == 0)) {
            # TODO: Clean up, but need to handle datasets with no arrays
            pivot_spec <- combined_metadata |>
                dplyr::mutate(
                    axis_name = NA_character_,
                    axis_values = vector("list", nrow(combined_metadata))
                )
        } else {
            pivot_spec <- combined_metadata |>
                tidyr::unnest(.data$axis_values, keep_empty = TRUE)
        }
        pivot_spec <- pivot_spec |>
            dplyr::transmute(
                .value = paste0(.data$type, "_value"),
                .name = ifelse(is.na(.data$axis_name), .data$name, .data$axis_name),
                var_name = .data$unexpanded_name,
                axis = .data$axis_values
            ) |>
            dplyr::mutate(.name = vctrs::vec_as_names(.data$.name, repair = "unique", quiet = TRUE))
    } else {
        pivot_spec <- combined_metadata |>
            dplyr::mutate(.value = paste0(.data$type, "_value")) |>
            dplyr::select(var_name = "unexpanded_name", "axis" = "axis_values", ".name" = "name", ".value")
    }

    # prepare data for reshaping
    data <- data |>
        dplyr::collect() |> # TODO: This makes us load the entire dataset from S3. Figure out a better way
        dplyr::mutate(
            axis = as.list(.data$axis) # vctrs gets in the way if we don't do this
        ) |>
        dplyr::select(-"var_type")

    out <- split_pivot_join(data, pivot_spec)

    if (array_strategy == "packed") {
        out <- pack_variables(out, combined_metadata, pivot_spec)
    }

    if (as_crunch_variables) out <- build_crunch_variables(out, combined_metadata)
    out
}

#' Read downloaded Crunch Lake data files
#'
#' Currently just a thin wrapper for reading the parquet file
#' as sent by the Crunch Lake in a "long" format. See [`cr_read_data()`] to
#' read the data in a 'tidy'-er wide format.
#'
#' @param x A folder path or destination from [`cr_export()`]
#' @param ... Saved for future expansion
#'
#' @export
cr_read_data_long <- function(x, ...) {
    if (crunch::is.dataset(x)) x <- download_lake_data_to_temp(x)
    if (is.DestinationSubpath(x)) {
        fs <- cr_arrow_fs(x$destination, ...)
        arrow::open_dataset(fs$cd(paste0(x$path, "/data")))
    } else {
        arrow::read_parquet(verify_file_path(x, "data.parquet"))
    }
}



# Having trouble convincing pivot_wider that there it doesn't need to make
# list columns
# Here is the natural way to write this (but this is too slow)
pivot_take_first <- function(data, spec) {
    tidyr::pivot_wider_spec(
        data,
        spec,
        # Haven't been able to convince tidyr that we will only have 1 item for some reason
        # So we need this to convert to non-list columns
        values_fn = dplyr::first
    )
}

# So here is a faster work around
# See https://forum.posit.co/t/convincing-tidyr-pivot-wider-that-my-spec-is-unique-so-that-it-doesnt-create-list-columns/210561
# for a synthetic benchmark and maybe someone will help with a better solution
split_pivot_join <- function(data, spec) {
    purrr::reduce(c("categorical", "numeric", "datetime", "text"), function(already_pivoted, type) {
        type_var <- paste0(type, "_value")
        type_spec <- spec |>
            dplyr::filter(.data$.value == type_var)

        if (nrow(type_spec) == 0) return(already_pivoted)

        type_data <- data |>
            dplyr::filter(.data$var_name %in% unique(type_spec$var_name)) |>
            dplyr::select(".row_id" = "row_id", "var_name", "axis", dplyr::all_of(type_var))

        type_pivoted <- tidyr::pivot_wider_spec(type_data, type_spec)

        if (is.null(already_pivoted)) return(type_pivoted)
        dplyr::full_join(already_pivoted, type_pivoted, by = ".row_id")
    }, .init = NULL) |>
        dplyr::select(dplyr::all_of(spec$.name), ".row_id")

}

build_crunch_variables <- function(data, combined_md) {
    column_attempts <- pmap_check(combined_md, function(name, type, axes, ...) {
        tibble::tibble(!!name := typed_crunch_variable(
            data[[name]],
            type = type,
            axes = axes,
            ...
        ))
    }, .message = "{.arg {name}} could not be created")

    errors <- purrr::keep(column_attempts, rlang::is_error)
    check_collect("Invalid crunch variable metadata (variables will be dropped)", !!!errors, severity = "warning")

    purrr::discard(column_attempts, rlang::is_error) |>
        dplyr::bind_cols()
}

array_lookup_info <- function(data, array_strategy) {
    if (is.null(data$axes[[1]])) {
        out <- data |>
            dplyr::mutate(unexpanded_name = name, axis_values = vctrs::list_of(NULL, .ptype = ""))
        return(out)
    }

    if (length(data$axes) == 1) {
        expanded_axes <- data$axes[[1]][[1]] |>
            dplyr::transmute(
                axis_values = as.list(value),
                axis_name = value,
                axis_label = label
            )
    } else {
        expanded_axes <- dplyr::cross_join(
            data$axes[[1]][[1]],
            data$axes[[1]][[2]],
            suffix = c("_outer", "_inner")
        ) |>
            dplyr::transmute(
                axis_values = purrr::map2(value_outer, value_inner, c),
                axis_name = paste0(.data$value_outer, "_", .data$value_inner),
                axis_label = paste0(.data$label_outer, " >> ", .data$label_inner)
                # TODO: Description (if present) too?
            )
    }

    if (array_strategy == "packed") {
        data$axis_values <- list(expanded_axes[, c("axis_values", "axis_name")])
        data$unexpanded_name <- data$name
        return(data)
    }
    data$axes <- list(NULL)

    if (array_strategy == "qualified") {
        expanded_axes <- expanded_axes |> dplyr::mutate(axis_name = paste0(data$name, "_", .data$axis_name))
    }

    dplyr::cross_join(data, expanded_axes) |>
        dplyr::mutate(
            unexpanded_name = name,
            name = .data$axis_name,
            label = paste0(.data$label, " >> ", .data$axis_label)
        ) |>
        dplyr::select(-"axis_name", -"axis_label")
}


pack_variables <- function(data, combined_metadata, pivot_spec) {
    purrr::pmap_dfc(combined_metadata, function(name, axes, ...) {
        if (is.null(axes)) return(data[name])

        subvars <- pivot_spec |> dplyr::filter(.data$var_name == name)
        if (all(lengths(subvars$axis)) == 1) {
            out <- tibble::tibble(!!name := data[subvars$.name] |> setNames(unlist(subvars$axis)))
        } else {
            subvars$outer <- purrr::map_chr(subvars$axis, 1)
            subvars$inner <- purrr::map_chr(subvars$axis, 2)
            inner_names <- unique(subvars$inner)
            out <- lapply(unique(subvars$outer), function(o_name) {
                this_inner <- subvars |> dplyr::filter(.data$outer == o_name)
                tibble::tibble(o_name := data[this_inner[[".name"]]] |> setNames(this_inner[["inner"]]))
            }) |> setNames(unique(subvars$outer)) |>
                tibble::as_tibble()
        }
        return(out)
    })
}
