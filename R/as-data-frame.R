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
#' If you call `as.data.frame` on a `CrunchDataset` with `force = TRUE`, you
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
#' @param include.hidden logical: should hidden variables be included? (default: `FALSE`)
#' @param ... additional arguments passed to `as.data.frame` (default method).
#' @return When called on a `CrunchDataset`, the method returns an object of
#' class `CrunchDataFrame` unless `force = TRUE`, in which case the return is a
#' `data.frame`. For `CrunchDataFrame`, the method returns a `data.frame`.
#' @seealso [as.vector()]
#' @name dataset-to-R
NULL

#' @rdname dataset-to-R
#' @export
as.data.frame.CrunchDataset <- function (x,
                                         row.names = NULL,
                                         optional = FALSE,
                                         force = FALSE,
                                         categorical.mode = "factor",
                                         include.hidden = FALSE,
                                         row.order = NULL,
                                         ...) {
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
    on.exit(unlink(tmp))
    write.csv(ds, tmp, categorical = "id")
    # TODO: use variableMetadata to provide all `colClasses`?
    # meta <- variableMetadata(ds)
    ds_out <- read.csv(tmp, stringsAsFactors = FALSE)
    return(csvToDataFrame(ds_out, x))
}

csvToDataFrame <- function (csv_df, crdf) {
    ds <- attr(crdf, "crunchDataset")
    mode <- attr(crdf, "mode")
    ## Use `variableMetadata` to avoid a GET on each variable entity for
    ## categories and subvariables
    ## Subset variableMetadata on the urls of the variables in the ds in case
    ## `ds` has only a subset of variables
    ds@variables <- variableMetadata(ds)[urls(allVariables(ds))]
    ## CrunchDataFrames contain both server variables and local variables.
    ## Iterate over the names of crdf to preserve the desired order.
    ## Nest individual columns in a list and then unlist all because array
    ## variables can return multiple columns
    out <- unlist(lapply(names(crdf), function (a) {
        v <- ds[[a]]
        if (is.null(v)) {
            ## Not in the dataset, so it exists only in the CRDF. Get it there.
            return(structure(list(crdf[[a]]), .Names=a))
        } else if (is.Array(v)) {
            ## Find the subvar columns in the csv_df and parse them as categorical
            cp <- columnParser("categorical")
            sub_a <- aliases(subvariables(v))
            return(structure(lapply(csv_df[sub_a], cp, v, mode), .Names=sub_a))
        } else {
            cp <- columnParser(type(v))
            return(structure(list(cp(csv_df[[a]], v, mode)), .Names=a))
        }
    }), recursive=FALSE)
    ## Wrap that list of columns in a data.frame structure
    return(structure(out, class="data.frame", row.names=c(NA, -nrow(ds))))
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
#' @name catalog-dataframes
#' @export
as.data.frame.VariableCatalog <- function (x,
                                           row.names = NULL,
                                           optional = FALSE,
                                           keys = c("alias", "name", "type"),
                                           ...) {
    catalogToDataFrame(x, keys = keys, row.names = row.names, ...)
}

#' @rdname catalog-dataframes
#' @export
as.data.frame.ShojiCatalog <- function (x,
                                        row.names = NULL,
                                        optional = FALSE,
                                        ...) {
    catalogToDataFrame(x, row.names = row.names, ...)
}

#' @rdname catalog-dataframes
#' @export
as.data.frame.BatchCatalog <- function (x,
                                        row.names = NULL,
                                        optional = FALSE,
                                        keys = c("id", "status"),
                                        ...) {
    catalogToDataFrame(x, keys = keys, row.names = row.names, ...)
}

#' @rdname catalog-dataframes
#' @export
as.data.frame.FilterCatalog <- function (x,
                                         row.names = NULL,
                                         optional = FALSE,
                                         keys = c("name", "id", "is_public"),
                                         ...) {
    catalogToDataFrame(x, keys = keys, row.names = row.names, ...)
}

#' @rdname catalog-dataframes
#' @export
as.data.frame.ProjectCatalog <- function (x,
                                         row.names = NULL,
                                         optional = FALSE,
                                         keys = c("name", "id", "description"),
                                         ...) {
    catalogToDataFrame(x, keys = keys, row.names = row.names, ...)
}
