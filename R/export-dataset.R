#' Export a dataset to a file
#'
#' This function allows you to write a CrunchDataset to a .csv or SPSS .sav file.
#'
#' @param dataset CrunchDataset, which may have been subsetted with a filter
#' expression on the rows and a selection of variables on the columns.
#' @param file character local filename to write to
#' @param format character export format: currently supported values are "csv"
#' and "spss".
#' @param categorical character: export categorical values to CSV as category
#' "name" (default) or "id". Ignored by the SPSS exporter.
#' @param na Similar to the argument in [utils::write.table()], 'na'
#' lets you control how missing values are written into the CSV file.
#' Supported values are:
#' 1. `NULL`, the default, which means that
#' categorical variables will have the category name or id as the value, and
#' numeric, text, and datetime variables will have the missing reason string;
#' 1. A string to use for missing values.
#' 1. `""` means that empty cells will be written for missing values for all types.
#' @param varlabel For SPSS export, which Crunch metadata field should be used
#' as variable labels? Default is "name", but "description" is another valid
#' value.
#' @param include.hidden logical: should hidden variables be included? (default: `FALSE`)
#' @param ... additional options. See the API documentation. Currently supported
#' boolean options include 'include_personal' for personal variables (default:
#' `FALSE`) and 'prefix_subvariables' for SPSS format: whether to include
#' the array variable's name in each of its subvariables "varlabels" (default:
#' `FALSE`).
#' @param x (for write.csv) CrunchDataset, which may have been subsetted with a filter
#' expression on the rows and a selection of variables on the columns.
#'
#' @aliases write.csv
#'
#' @return Invisibly, `file`.
#' @export
exportDataset <- function(dataset, file, format = c("csv", "spss"),
                          categorical = c("name", "id"), na = NULL,
                          varlabel = c("name", "description"),
                          include.hidden = FALSE, ...) {
    exporters <- crGET(shojiURL(dataset, "views", "export"))
    format <- match.arg(format, choices = names(exporters))
    export_url <- exporters[[format]]

    body <- list(filter = zcl(activeFilter(dataset)))
    ## Add this after so that if it is NULL, the "where" key isn't present
    body$where <- variablesFilter(dataset, include.hidden)
    ## Assemble options
    opts <- list(...)
    if (format == "csv") {
        opts$use_category_ids <- match.arg(categorical) == "id"
        if (!is.null(na)) {
            stopifnot(is.character(na), length(na) == 1)
            ## match.arg fails if na="" because pmatch fails
            opts$missing_values <- na
        }
    } else if (format == "spss") {
        opts$var_label_field <- match.arg(varlabel)
    }
    if (length(opts)) {
        body$options <- opts
    }

    result <- crPOST(export_url, body = toJSON(body))
    file <- crDownload(result, file)
    invisible(file)
}

variablesFilter <- function(dataset, include.hidden = FALSE) {
    ## Check to see if we have a subset of variables in `dataset`.
    ## If so, return a Crunch expression to filter them
    allvars <- allVariables(dataset)
    ## TODO: fix Variable catalog so that it doesn't pop off its "relative"
    ## query from self. Adding it here so that we hit cache.
    dsvars <- ShojiCatalog(crGET(self(allvars), query = list(relative = "on")))
    if (include.hidden || (length(allvars) != length(dsvars))) {
        v <- structure(lapply(urls(allvars), function(x) list(variable = x)),
            .Names = ids(allvars)
        )
        ## Make sure that duplicate variables haven't been referenced (surely
        ## by accident).
        ## TODO: move this to a ShojiCatalog subset method?
        dupes <- duplicated(names(v))
        if (any(dupes)) {
            dup.aliases <- unique(aliases(allvars[dupes]))
            halt(
                pluralize("Duplicate variable reference", length(dup.aliases)), ": ",
                serialPaste(dup.aliases)
            )
        }
        return(list(`function` = "select", args = list(list(map = v))))
    }
    ## Else, return NULL
    return(NULL)
}

#' @rdname exportDataset
#' @export
setMethod("write.csv", "CrunchDataset", function(x, ...) {
    exportDataset(x, ..., format = "csv")
})
