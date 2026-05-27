#' Read downloaded Crunch Lake metadata/schema files into a friendly format
#'
#' A 'tidy' R representation of the information contained in the metadata, schema
#' and folders files provided by the Crunch Lake. See [`cr_read_metadata_raw()`],
#' [`cr_read_schema()`], and [`cr_read_folders()`]  to read the raw files.
#'
#' @param x A folder path or destination from [`cr_export()`]
#' @param ... Saved for future expansion
#'
#' @export
cr_read_meta <- function(x, ...) {
    if (crunch::is.dataset(x)) {
        metafiles <- download_lake_metadata(x)
        md <- metafiles$metafiles$metadata
        schema <- metafiles$metafiles$schema
        folders <- metafiles$metafiles$folders
    } else {
        md <- cr_read_metadata_raw(x, ...)
        schema <- cr_read_schema(x, ...)
        folders <- cr_read_folders(x)
    }

    # Build Doug's combined schema+metadata
    info <- purrr::map_dfr(names(md), function(nm) {
        schema_var <- schema[[nm]]
        md_var <- md[[nm]]

        adjusted_type <- adjust_meta_schema_type(schema_var, md_var)
        consolidated_values <- combine_meta_schema_values(schema_var, md_var)
        consolidated_axes <- combine_meta_schema_axes(schema_var, md_var)

        tibble::tibble(
            name = schema_var$name,
            type = adjusted_type,
            label = md_var$label,
            description = md_var$description,
            notes = md_var$notes,
            values = list(consolidated_values),
            axes = list(consolidated_axes)
        )
    })

    # Order based on folders and add path
    folder_var_order(folders) |>
        dplyr::right_join(info, by = "name") |>
        dplyr::relocate(dplyr::one_of("path"), .after = dplyr::last_col())
}


#' Read downloaded Crunch Lake metadata/schema files
#'
#' Currently just thin wrappers for reading the JSON/parquet files
#' as sent by the Crunch Lake. See [`cr_read_meta()`] for
#' a friendlier way to load this.
#'
#' @param x A folder path or destination from [`cr_export()`]
#' @param ... Saved for future expansion
#'
#' @export
cr_read_metadata_raw <- function(x, ...) {
    if (is.DestinationSubpath(x)) {
        return(read_json_from_subdestination(x, "/meta/metadata.json", ...))
    }
    jsonlite::read_json(verify_file_path(x, "metadata.json"))
}

#' @export
#' @rdname cr_read_metadata_raw
cr_read_schema <- function(x, ...) {
    if (is.DestinationSubpath(x)) {
        return(read_json_from_subdestination(x, "/meta/schema.json", ...))
    }
    jsonlite::read_json(verify_file_path(x, "schema.json"))

}

#' @export
#' @rdname cr_read_metadata_raw
cr_read_folders <- function(x, ...) {
    if (is.DestinationSubpath(x)) {
        return(read_json_from_subdestination(x, "/meta/folders.json", ...))
    }
    out <- jsonlite::read_json(verify_file_path(x, "folders.json"))
    # Assume once it's in the real download API the shoji layers will be peeled off
    if ("element" %in% names(out) && out$element == "shoji:view") {
        out <- out$value$content
    }
    out
}


adjust_meta_schema_type <- function(schema_var, md_var) {
    if (schema_var$type != "array") return(schema_var$type)
    return(schema_var$value$type)
}

combine_meta_schema_values <- function(schema_var, md_var) {
    if (is.null(md_var$values)) {
        return(NULL)
    }
    purrr::map_dfr(md_var$values, ~tibble::tibble(
        value = .$value,
        label = .$label,
        missing = .$missing,
        scale = .$scale %||% NA_real_,
        selected = .$selected %||% NA,
        date = .$date %||% NA_character_
    ))
}

combine_meta_schema_axes <- function(schema_var, md_var) {
    if (is.null(md_var$axes)) {
        return(NULL)
    }

    purrr::map(
        unname(md_var$axes),
        ~.$values |>
            dplyr::bind_rows() |>
            dplyr::transmute(
                value = .data$value,
                label = .data$label,
                description = if ("description" %in% names(.data)) .data$description else NA_character_
            )
    )
}

folder_var_order <- function(folders) {
    dplyr::bind_rows(
        inner_folder_var_order(folders$public, "/public/"),
        inner_folder_var_order(folders$personal, "/personal/"),
        inner_folder_var_order(folders$shared, "/shared/"),
        inner_folder_var_order(folders$hidden, "/hidden/"),
        inner_folder_var_order(folders$secure, "/secure/")
    )
}

inner_folder_var_order <- function(x, cur_path) {
    purrr::map_dfr(x, function(item) {
        if (item$type == "variable") {
            tibble::tibble(name = item$name, path = cur_path)
        } else {
            inner_folder_var_order(item$children, cur_path = paste0(cur_path, item$name, "/"))
        }
    })
}


read_json_from_subdestination <- function(dsubpath, filepath, ...) {
    # TODO: Is there a better way to do this?
    fs <- cr_arrow_fs(dsubpath$destination, ...)
    file <- fs$OpenInputFile(paste0(dsubpath$path, filepath))
    file$Read()$data() |> rawToChar() |> jsonlite::fromJSON(simplifyVector = FALSE)
}
