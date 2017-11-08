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
#' @param x a CrunchDataset
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
#' which case the return is a `data.frame`.
#' @name dataset-to-R
NULL

#' @rdname dataset-to-R
#' @export
as.data.frame.CrunchDataset <- function (x, debug = FALSE, row.names = NULL, optional = FALSE,
                                        force=FALSE, categorical.mode = "factor",
                                        include.hidden = FALSE,
                                        row.order = NULL, ...) {
    out <- CrunchDataFrame(x, row.order = row.order,
                           categorical.mode = categorical.mode,
                           include.hidden = include.hidden)
    if (force) {
        out <- as.data.frame(out, debug = debug)
    }
    return(out)
}

#' @rdname dataset-to-R
#' @export
as.data.frame.CrunchDataFrame <- function (x, debug = FALSE,  row.names = NULL, optional = FALSE, ...) {
    if(debug) browser()
    ds <- attr(x, "crunchDataset")
    tmp <- tempfile()
    write.csv(ds, tmp)
    ds_out <- read.csv(tmp, stringsAsFactors = FALSE)
    ds_out[] <- unlist(lapply(ds, coerceVariable, ds_out), recursive = FALSE)

    var_names <- names(x)


    ## Crunch Dataframes contain both server variables and local variables this
    ## ensures that both are returned in the proper order.
    out <- lapply(var_names, function(v){
        if (v %in% names(ds_out)) {
            if (tuple(ds[[v]])$discarded) {
                warning("Variable ", v, " is hidden", call.=FALSE)
            }
            return(ds_out[[v]])
        } else {
            return(x[[v]])
        }
    })
    names(out) <- var_names
    return(structure(out, class="data.frame", row.names=c(NA, -nrow(ds))))
}

coerceVariable <- function(var, df){
    if (is.Array(var)) {
        out <- lapply(subvariables(var), function(x){
            factor(df[[x$name]], levels = names(categories(var)))
        })
    } else if (is.Numeric(var)) {
        out <- list(as.numeric(df[[name(var)]]))
    } else if (is.Categorical(var)) {
        out <- list(factor(df[[name(var)]], levels = names(categories(var))))
    } else if (is.Datetime(var)) {
        out <- list(coercion_fun = as.Date(df[[name(var)]]))
    } else if (is.Text(var)) {
        out <- list(coercion_fun = as.character(df[[name(var)]]))
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
