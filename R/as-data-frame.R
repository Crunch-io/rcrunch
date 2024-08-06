#' as.data.frame method for CrunchDataset
#'
#' This method is defined principally so that you can use a `CrunchDataset` as
#' a `data` argument to other R functions (such as [stats::lm()]) without
#' needing to download the whole dataset. You can, however, choose to download
#' a true `data.frame`.
#'
#' By default, the `as.data.frame` method for `CrunchDataset` does not return a
#' `data.frame` but instead `CrunchDataFrame`, which behaves like  a
#' `data.frame` without bringing the whole dataset into memory.
#' When you access the variables of a `CrunchDataFrame`,
#' you get an R vector, rather than a `CrunchVariable`. This allows modeling functions
#' that require select columns of a dataset to retrieve only those variables from
#' the remote server, rather than pulling the entire dataset into local
#' memory.
#'
#' If you call `as.data.frame()` on a `CrunchDataset` with `force = TRUE`, you
#' will instead get a true `data.frame`. You can also get this `data.frame` by
#' calling `as.data.frame` on a `CrunchDataFrame` (effectively calling
#' `as.data.frame` on the dataset twice)
#'
#' When a `data.frame` is returned, the function coerces Crunch Variable
#' values into their R equivalents using the following rules:
#'
#' * Numeric variables become numeric vectors
#' * Text variables become character vectors
#' * Datetime variables become either `Date` or `POSIXt` vectors
#' * Categorical variables become either factors with
#' levels matching the Crunch Variable's categories (the default), or, if
#' `categorical.mode` is specified as "id" or "numeric", a numeric vector of
#' category ids or numeric values, respectively
#' * Array variables (Categorical Array, Multiple Response) are decomposed into
#' their constituent categorical subvariables. An array with three subvariables,
#' for example, will result in three columns in the `data.frame`.
#'
#' Column names in the `data.frame` are the variable/subvariable aliases.
#'
#' @param x a `CrunchDataset` or `CrunchDataFrame`
#' @param row.names part of `as.data.frame` signature. Ignored.
#' @param optional part of `as.data.frame` signature. Ignored.
#' @param force logical: actually coerce the dataset to `data.frame`, or
#' leave the columns as unevaluated promises. Default is `FALSE`.
#' @param row.order vector of indices. Which, and their order, of the rows of
#'  the dataset should be presented as (default: `NULL`). If `NULL`, then the
#'  Crunch Dataset order will be used.
#' @param categorical.mode what mode should categoricals be pulled as? One of
#' factor, numeric, id (default: factor)
#' @param include.hidden logical: should hidden variables be included? (default: `TRUE`)
#' @param array_strategy Strategy to import array variables: "alias" (the default)
#' reads them as flat variables with the subvariable aliases, unless there are duplicate
#' aliases in which case they are qualified in brackets after the array alias,
#' like "array_alias\[subvar_alias\]". "qualified_alias" always uses the bracket notation.
#' "packed" reads them in what the tidyverse calls "packed" data.frame columns, with the
#' alias from the array variable, and subvariables as the columns of the data.frame.
#' @param verbose Whether to output a message to the console when subvariable aliases
#' are qualified when array_strategy="alias" (defaults to TRUE)
#' @param ... additional arguments passed to `as.data.frame` (default method).
#' @return When called on a `CrunchDataset`, the method returns an object of
#' class `CrunchDataFrame` unless `force = TRUE`, in which case the return is a
#' `data.frame`. For `CrunchDataFrame`, the method returns a `data.frame`.
#' @seealso [as.vector()]
#' @name dataset-to-R
NULL

#' @rdname dataset-to-R
#' @export
as.data.frame.CrunchDataset <- function(x,
                                        row.names = NULL,
                                        optional = FALSE,
                                        force = FALSE,
                                        categorical.mode = "factor",
                                        row.order = NULL,
                                        include.hidden = TRUE,
                                        ...) {
    out <- CrunchDataFrame(x,
        row.order = row.order,
        categorical.mode = categorical.mode,
        include.hidden = include.hidden
    )
    if (force) {
        out <- as.data.frame(out, ...)
    }
    return(out)
}

#' @rdname dataset-to-R
#' @importFrom utils read.csv
#' @export
as.data.frame.CrunchDataFrame <- function(x,
                                          row.names = NULL,
                                          optional = FALSE,
                                          include.hidden = attr(x, "include.hidden"),
                                          array_strategy = c("alias", "qualified_alias", "packed"),
                                          verbose = TRUE,
                                          ...) {
    array_strategy <- match.arg(array_strategy)
    ds <- attr(x, "crunchDataset")
    tmp <- tempfile()
    on.exit(unlink(tmp))
    write.csv(
        ds,
        tmp,
        categorical = "id",
        header_field = "qualified_alias",
        missing_values = "",
        include.hidden = include.hidden
    )

    parsing_info <- csvColInfo(ds, verbose = verbose && array_strategy == "alias")

    # guessing has been good enough (and distinguishes between Date and POSIXct class for us)
    # except for text variables, so continue to guess the parsing info for all columns besides text
    col_classes <- setNames(
        ifelse(parsing_info$var_type == "text", "character", NA_character_),
        parsing_info$qualified_alias
    )

    ds_out <- read.csv(
        tmp,
        stringsAsFactors = FALSE,
        check.names = FALSE,
        colClasses = col_classes,
        na.strings = ""
    )
    dup_csv_names <- duplicated(names(ds_out))
    if (any(dup_csv_names)) {
        stop(
            "csv has duplicate column headers, cannot parse: ",
            paste0(unique(names(ds_out)[dup_csv_names]), collapse = ", ")
        )
    }
    return(csvToDataFrame(ds_out, x, parsing_info, array_strategy, categorical.mode = attr(x, "mode")))
}

csvColInfo <- function(ds, verbose = TRUE) {
    # Get variable metadata for variables included in the export
    meta <- variableMetadata(ds)[urls(allVariables(ds))]
    flattened_meta <- flattenVariableMetadata(meta)

    orig_aliases <- aliases(flattened_meta)
    parent_aliases <- vapply(flattened_meta, function(x) x$parent_alias %||% NA_character_, character(1))
    qualified_aliases <- ifelse(
        is.na(parent_aliases),
        orig_aliases,
        paste0(parent_aliases, "[", orig_aliases, "]")
    )
    # cond_qualified_aliases are only qualified if there are duplicates
    dup_aliases <- orig_aliases[duplicated(orig_aliases)]
    cond_qualified_aliases <- ifelse(orig_aliases %in% dup_aliases, qualified_aliases, orig_aliases)
    out <- data.frame(
        orig_alias = orig_aliases,
        parent_alias = parent_aliases,
        qualified_alias = qualified_aliases,
        cond_qualified_alias = cond_qualified_aliases,
        var_type = types(flattened_meta)
    )
    out <- out[!out$var_type %in% ARRAY_TYPES, ]

    if (verbose) {
        msg_rows <- out$cond_qualified_alias != out$orig_alias
        if (any(msg_rows)) {
            alias_info <- paste0(out$orig_alias[msg_rows], " -> ", out$cond_qualified_alias[msg_rows])
            message(
                "Some column names are qualified because there were duplicate aliases ",
                "in dataset:\n", paste0(alias_info, collapse = ", ")
            )
        }
    }

    attr(out, "meta") <- meta
    out
}

csvToDataFrame <- function(csv_df,
                           cr_data,
                           parsing_info,
                           array_strategy = c("alias", "qualified_alias", "packed"),
                           categorical.mode = "factor") {
    array_strategy <- match.arg(array_strategy)
    meta <- attr(parsing_info, "meta")
    ## CrunchDataFrames contain both server variables and local variables.
    var_order <- if (inherits(cr_data, "CrunchDataFrame")) names(cr_data) else aliases(allVariables(cr_data))
    ## Iterate over the names of cr_data to preserve the desired order.
    ## Nest everything in an extra layer of lists because one layer is removed
    out <- unlist(lapply(var_order, function(a) {
        meta_idx <- match(a, aliases(meta))
        v <- if (!is.na(meta_idx)) meta[[meta_idx[1]]] else NULL
        if (is.null(v)) {
            ## Not in the dataset, so it exists only in the CRDF. Get it there.
            return(structure(list(cr_data[[a]]), .Names = a))
        } else if (type(v) %in% ARRAY_TYPES) {
            ## Find the subvar columns in the csv_df and parse them as categorical
            if (type(v) == "numeric_array") {
                cp <- numericCsvParser
            } else {
                cp <- columnParser("categorical")
            }
            subvar_info <- parsing_info[!is.na(parsing_info$parent_alias) & parsing_info$parent_alias == alias(v), ]
            cols <- csv_df[, subvar_info$qualified_alias]
            if (array_strategy == "alias"){
                return(structure(lapply(cols, cp, v, categorical.mode), .Names = subvar_info$cond_qualified_alias))
            } else if (array_strategy == "qualified_alias") {
                return(structure(lapply(cols, cp, v, categorical.mode), .Names = subvar_info$qualified_alias))
            } else { # array_strategy==packed
                # Extra list layer to hold the array variable's alias
                return(structure(
                    list(
                        structure(
                            lapply(cols, cp, v, categorical.mode),
                            class = "data.frame",
                            .Names = subvar_info$orig_alias,
                            row.names = c(NA, -nrow(csv_df))
                        )
                    ),
                    .Names = alias(v)
                ))
            }
        } else {
            type <- type(v)
            cp <- switch(type, "numeric" = numericCsvParser, "text" = textCsvParser, columnParser(type))
            return(structure(list(cp(csv_df[[a]], v, categorical.mode)), .Names = a))
        }
    }), recursive = FALSE)

    ## Wrap that list of columns in a data.frame structure
    return(structure(out, class = "data.frame", row.names = c(NA, -nrow(csv_df))))
}

# We pass missing_values to export so no longer have to worry about finding text
# in a numeric variable
numericCsvParser <- function(col, ...) col

# When data comes from a csv it should already be text (and definitely won't be
# a list with missing reasons included like JSON's text columnParser)
textCsvParser <- function(col, ...) col


#' as.data.frame method for catalog objects
#'
#' This method gives you a view of a catalog, such as a `VariableCatalog`, as a
#' `data.frame` in order to facilitate further exploration.
#'
#' Modifying the `data.frame` produced by this function will not update the
#' objects on the Crunch server. Other methods exist for updating the metadata
#' in the variable catalog, for example. See `vingette("variables", package = "crunch")`.
#'
#' @param x A catalog object
#' @param row.names A character vector of elements to use as row labels for the
#' resulting data.frame, or `NULL`, the default, adds no row labels.
#' @param optional part of `as.data.frame` signature. Ignored.
#' @param keys A character vector of the catalog attributes that you
#' would like included in the data.frame. To include all attributes, set keys to
#' `TRUE`, which is the default for some catalogs. Other catalog classes specify
#' a narrower default:
#' * VariableCatalog: `c("alias", "name", "type")`
#' * BatchCatalog: `c("id", "status")`
#' * FilterCatalog: `c("name", "id", "is_public")`
#' @param ... Additional arguments passed to `data.frame`
#' @return A `data.frame` including metadata about each entity contained in the
#' catalog. The fields in the data.frame match the `keys` argument
#' provided to the function, and each row represents a entity.
#' @examples
#' \dontrun{
#' ds <- loadDataset("iris")
#' vars <- variables(ds)
#' var_df <- as.data.frame(vars, keys = TRUE)
#' # With row names
#' as.data.frame(vars, row.names = urls(vars))
#' }
#'
#' @name catalog-dataframes
#' @export
as.data.frame.VariableCatalog <- function(x,
                                          row.names = NULL,
                                          optional = FALSE,
                                          keys = c("alias", "name", "type"),
                                          ...) {
    catalogToDataFrame(x, keys = keys, row.names = row.names, ...)
}

#' @rdname catalog-dataframes
#' @export
as.data.frame.ShojiCatalog <- function(x,
                                       row.names = NULL,
                                       optional = FALSE,
                                       ...) {
    catalogToDataFrame(x, row.names = row.names, ...)
}

#' @rdname catalog-dataframes
#' @export
as.data.frame.BatchCatalog <- function(x,
                                       row.names = NULL,
                                       optional = FALSE,
                                       keys = c("id", "status"),
                                       ...) {
    catalogToDataFrame(x, keys = keys, row.names = row.names, ...)
}

#' @rdname catalog-dataframes
#' @export
as.data.frame.FilterCatalog <- function(x,
                                        row.names = NULL,
                                        optional = FALSE,
                                        keys = c("name", "id", "is_public"),
                                        ...) {
    catalogToDataFrame(x, keys = keys, row.names = row.names, ...)
}

#' @rdname catalog-dataframes
#' @export
as.data.frame.UserCatalog <- function(x, row.names = NULL,
                                      optional = FALSE,
                                      keys = c("name", "email", "teams", "collaborator"),
                                      ...) {
    catalogToDataFrame(x, keys = keys, row.names = row.names, ...)
}
