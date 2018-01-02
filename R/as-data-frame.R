#' as.data.frame method for CrunchDataset
#'
#' This method is defined principally so that you can use a CrunchDataset as
#' a `data` argument to other R functions (such as [stats::lm()]). By default,
#' the function does not return a `data.frame` but instead `CrunchDataFrame`,  which
#' behaves similarly to a `data.frame` without bringing the whole dataset into memory.
#' When you access the variables of a `CrunchDataFrame`,
#' you get an R vector, rather than a `CrunchVariable`. This allows modeling functions
#' that require select columns of a dataset to retrieve only those variables from
#' the remote server, rather than pulling the entire dataset into local
#' memory. You can override this behavior by passing `force = TRUE`, which will
#' cause the function to return a traditional `data.frame`.
#'
#' When a traditional `data.frame` is returned the function coerces Crunch Variable
#' values into their R equivalents using the following rules:
#'
#' * Numeric variables become numeric vectors
#' * Text variables become character vectors
#' * Categorical, Multiple Response, and Catagorical Arrays are coerced to factors whose
#' levels match the Crunch Variable's categories
#' * Array variables are decomposed into their constituent subvariables. So if an
#' array variable had three subvariables it will be decomposed into three factor
#' columns in the resulting dataframe
#'
#' @param x a CrunchDataset or Dataframe
#' @param row.names part of as.data.frame signature. Ignored.
#' @param optional part of as.data.frame signature. Ignored.
#' @param force logical: actually coerce the dataset to `data.frame`, or
#' leave the columns as unevaluated promises. Default is `FALSE`.
#' @param row.order vector of indices. Which, and their order, of the rows of
#'  the dataset should be presented as (default: `NULL`). If `NULL`, then the
#'  Crunch Dataset order will be used.
#' @param categorical.mode what mode should categoricals be pulled as? One of
#' factor, numeric, id (default: factor)
#' @param include.hidden should hidden variables be included? (default: `FALSE`)
#' @param ... additional arguments passed to `as.data.frame` (default method).
#' @return an object of class `CrunchDataFrame` unless `force`, in
#' which case the return is a `data.frame`. For `CrunchDataFrame` the function returns
#' a regular dataframe.
#' @name dataset-to-R
NULL

#' @rdname dataset-to-R
#' @export
as.data.frame.CrunchDataset <- function (x, row.names = NULL, optional = FALSE,
    force=FALSE, categorical.mode = "factor",
    include.hidden = FALSE,
    row.order = NULL, ...) {
    out <- CrunchDataFrame(x, row.order = row.order,
        categorical.mode = categorical.mode,
        include.hidden = include.hidden)
    if (force) {
        out <- as.data.frame(out)
    }
    return(out)
}

#' @rdname dataset-to-R
#' @importFrom utils read.csv
#' @export
as.data.frame.CrunchDataFrame <- function (x,
    row.names = NULL,
    optional = FALSE,
    ...) {
    ds <- attr(x, "crunchDataset")
    tmp <- tempfile()
    csv_mode = ifelse(attr(x, "mode") == "factor", "name", "id")
    write.csv(ds, tmp, categorical = csv_mode)
    ds_out <- read.csv(tmp, stringsAsFactors = FALSE, colClasses = "character")
    unlink(tmp)
    return(csvToDataFrame(ds_out, x))
}

csvToDataFrame <- function(csv_df, crdf) {
    ds <- attr(crdf, "crunchDataset")
    csv_df[] <- unlist(lapply(ds[, names(ds)], coerceVariable, csv_df, crdf), recursive = FALSE)
    var_names <- lapply(names(crdf), function (v) {
        if (is.Array(ds[[v]])) {
            return(aliases(subvariables(ds[[v]])))
        } else {
            return(v)
        }
    })
    var_names <- unlist(var_names)
    ## Crunch Dataframes contain both server variables and local variables this
    ## ensures that both are returned in the proper order.
    out <- lapply(var_names, function (v) {
        if (v %in% names(csv_df)) {
            return(csv_df[[v]])
        } else {
            return(crdf[[v]])
        }
    })
    names(out) <- var_names
    return(structure(out, class="data.frame", row.names=c(NA, -nrow(ds))))
}

coerceFactor <- function (var_alias, cats, df, cdf) {
    # csv download doesn't accomodate numeric factor values, so in that case
    # we need to pull the variable in the typical way.
    mode <- attr(cdf, "mode")
    if (mode == "numeric") {
        return(cdf[[var_alias]])
    } else if (mode == "id") {
        return(suppressWarnings(as.numeric(df[[var_alias]])))
    } else {
        na_cats <- is.na(cats)
        v <- df[[var_alias]]
        levs <- names(cats)
        v[v %in% levs[na_cats]] <- NA
        v <- factor(v, levels = levs[!na_cats])
        return(v)
    }
}

coerceVariable <- function (var, df, cdf) {
    if (is.Array(var)) {
        out <- lapply(subvariables(var), function (x) {
            coerceFactor(x$alias, categories(var), df, cdf)
            })
    } else if (is.Numeric(var)) {
        out <- list(suppressWarnings(as.numeric(df[[alias(var)]]))) # To prevent NA coercion message
    } else if (is.Categorical(var)) {
        out <- list(coerceFactor(alias(var), categories(var), df, cdf))
    } else if (is.Datetime(var)) {
        out <- list(suppressWarnings(as.Date(df[[alias(var)]]))) # To prevent NA coercion warning
    } else if (is.Text(var)) {
        out <- list(as.character(df[[alias(var)]]))
    }
    return(out)
}


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
#' @rdname catalog-to-data-frame
#' @export
as.data.frame.VariableCatalog <- function (x, row.names = NULL,
    optional = FALSE,
    keys = c("alias", "name", "type"),
    ...) {
    catalogToDataFrame(x, keys = keys, row.names = row.names, ...)
}

#' @rdname catalog-to-data-frame
#' @export
as.data.frame.ShojiCatalog <- function (x, row.names = NULL,
    optional = FALSE,
    ...) {
    catalogToDataFrame(x, row.names = row.names, ...)
}

#' @rdname catalog-to-data-frame
#' @export
as.data.frame.BatchCatalog <- function (x, row.names = NULL,
    optional = FALSE,
    keys = c("id", "status"),
    ...) {
    catalogToDataFrame(x, keys = keys, row.names = row.names, ...)
}

#' @rdname catalog-to-data-frame
#' @export
as.data.frame.FilterCatalog <- function (x, row.names = NULL,
    optional = FALSE,
    keys = c("name", "id", "is_public"),
    ...) {
    catalogToDataFrame(x, keys = keys, row.names = row.names, ...)
}
