#' Export data & metadata from crunch lake
#'
#' @param ds A `CrunchDataset` or URI for a crunch dataset
#' @param ... Options passed to the API, see [https://crunch.io/api/reference/#post-/datasources/download/]()
#' for complete list.
#' @param path Path to where files should be saved, defaults to a directory with dataset's
#' name in the current directory
#' @param destination Optional data destination to export data to (see [`cr_destination()`])
#'
#' @returns If `destination` is unspecified, a string path to the folder containing
#' the downloaded files, or if it is a reference to the destination and path.
#' Both can have [`cr_read_data()`] or [`cr_read_meta()`] and other reading functions called on.
#' @export
#'
#' @examples
#' \dontrun{
#' files <- cr_export(ds, export_filter = list(variables = I(c("wave", "age"))))
#'
#' md <- files |> cr_read_meta()
#' data <- files |> cr_read_data()
#' }
cr_export <- function(ds, ..., path = NULL, destination = NULL) {
    stopifnot(crunch::is.dataset(ds) || is.character(ds))
    if (is.character(ds) && grepl("/dataset/", ds)) {
        ds <- crunch::loadDataset(ds)
    }
    if (is.null(path)) {
        if (is.character(ds)) stop("Must provide path if referencing datasource via URL")
        path <- file.path(crunch::name(ds))
    }
    stopifnot(is.character(path))

    body <- dataset_or_datasource_body(ds)
    if (...length() > 0) { # API doesn't accept `null`
        body$options <- list(...)
    }

    if (is.null(destination)) {
        dir.create(path, showWarnings = FALSE, recursive = TRUE)

        export_url <- crunch:::getAPIRoot() |>
            crunch::shojiURL("catalogs", "datasources") |>
            crunch::crGET() |>
            crunch:::ShojiObject() |>
            crunch::shojiURL("views", "download")

        handler <- download_lake_data_handler(path, ds)
    } else {
        if (!is.destination(destination)) cli::cli_abort("{.arg destination} must be a destination")

        export_url <- crunch:::getAPIRoot() |>
            crunch::shojiURL("catalogs", "datasources") |>
            crunch::crGET() |>
            crunch:::ShojiObject() |>
            crunch::shojiURL("views", "export")

        handler <- export_lake_data_handler(path, destination)
        body$datadestination <- crunch::self(destination)
        if (is.null(body$options)) body$options <- list()
        body$options$path <- path
    }

    body <- body |>
        crunch:::wrapView() |>
        crunch::toJSON()


    out <- crunch::crPOST(
        export_url,
        body = body,
        status.handlers = list(`202` = handler)
    )

    out
}

download_lake_data_to_temp <- function(ds, ...) {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    cr_export(ds, path = temp_dir, ...)
}

download_lake_data_handler <- function(path, ds) {
    function(response) {
        loc <- crunch:::locationHeader(response)
        ## Progress URL comes in a shoji:value
        progress_url <- crunch:::handleShoji(httr::content(response))
        ## Quick validation
        if (is.character(progress_url) && length(progress_url) == 1) {
            if (crunch::envOrOption("crunch.show.progress.url", FALSE, expect_lgl = TRUE)) {
                message(paste0("Checking progress at: ", progress_url))
            }

            final_poll_response <- pollProgressContent(
                progress_url,
                crunch::envOrOption("crunch.poll.wait", 0.5, expect_num = TRUE)
            )

            file_types <- names(final_poll_response$result)
            lapply(file_types, function(type) {
                url <- final_poll_response$result[[type]]
                file_name <- tryCatch(basename(gsub("\\?.+$", "", url)), error = function(e) {
                    warning("Not sure name to use for file, using `", type, "`")
                    type
                })
                httpcache::uncached(crunch:::crDownload(url, file.path(path, file_name)))
            })
            # Get folders manually until added to download API
            if (!"folders.json" %in% list.files(path)) {
                download_lake_folders(ds, path)
            }
            return(path)
        }
    }
}

export_lake_data_handler <- function(path, destination) {
    function(response) {
        loc <- crunch:::locationHeader(response)
        ## Progress URL comes in a shoji:value
        progress_url <- crunch:::handleShoji(httr::content(response))
        if (is.list(progress_url) && "progress" %in% names(progress_url)) progress_url <- progress_url$progress
        ## Quick validation
        if (is.character(progress_url) && length(progress_url) == 1) {
            if (crunch::envOrOption("crunch.show.progress.url", FALSE, expect_lgl = TRUE)) {
                message(paste0("Checking progress at: ", progress_url))
            }

            final_poll_response <- pollProgressContent(
                progress_url,
                crunch::envOrOption("crunch.poll.wait", 0.5, expect_num = TRUE)
            )

            return(DestinationSubpath(destination = destination, path = path))
        }
    }
}

# Manually download folders from API until it's added to export
download_lake_folders <- function(ds, path) {
    # Get datasouce id from dataset id
    datasource_export_url <- crunch:::getAPIRoot() |>
        crunch::shojiURL("catalogs", "datasources")

    if (!is.character(ds)) {
        datasource_url <- crunch::crGET(paste0(datasource_export_url, "?dataset_id=", crunch::id(ds))) |>
            purrr::pluck("index") |>
            names()
    } else {
        datasource_url <- ds
    }

    httpcache::uncached(crunch:::crDownload(paste0(datasource_url, "/folders/latest"), paste0(path, "/folders.json"))) |>
        normalizePath()
}


download_lake_metadata <- function(ds, ...) {
    stopifnot(crunch::is.dataset(ds) || is.character(ds))
    if (is.character(ds) && grepl("/dataset/", ds)) {
        ds <- crunch::loadDataset(ds)
    }

    # TODO: Can we support datasources too? Datasources also identified by the URI, but have different key
    # in request (datasource instead of dataset) Maybe Lake team could distinguish since URI would be unique?
    if (crunch::is.dataset(ds)) {
        body <- list(dataset = crunch::self(ds))
    } else if (grepl("/datasets/", ds)) {
        body <- list(dataset = ds)
    } else if (grepl("/datasources/", ds)) {
        body <- list(datasource = ds)
    } else {
        stop("Unexpected ds, must be a CrunchDataset or a URL to a dataset or datasource")
    }
    if (...length() > 0) { # API doesn't accept `null`
        body$options <- list(...)
    }
    body <- body |>
        crunch:::wrapView() |>
        crunch::toJSON()

    out <- crunch::crPOST(
        paste0(crunch::envOrOption("crunch.api"), "datasources/metafiles"),
        body = body,
        status.handlers = list(`202` = download_lake_metafiles_handler(), `200` = function(...) browser())
    )
    out

}

# --- TODO: This pattern should be generalized when I understand it better
download_lake_metafiles_handler <- function() {
    function(response) {
        loc <- crunch:::locationHeader(response)
        ## Progress URL comes in a shoji:value
        progress_url <- crunch:::handleShoji(httr::content(response))
        ## Quick validation
        if (is.character(progress_url) && length(progress_url) == 1) {
            if (crunch::envOrOption("crunch.show.progress.url", FALSE, expect_lgl = TRUE)) {
                message(paste0("Checking progress at: ", progress_url))
            }

            final_poll_response <- pollProgressContent(
                progress_url,
                crunch::envOrOption("crunch.poll.wait", 0.5, expect_num = TRUE)
            )

            final_poll_response$result
        }
    }
}

dataset_or_datasource_body <- function(ds) {
    if (crunch::is.dataset(ds)) {
        return(list(dataset = crunch::self(ds)))
    }
    if (is.character(ds) && grepl("/datasets/", ds)) {
        return(list(dataset = ds))
    }
    if (is.character(ds) && grepl("/datasources/", ds)) {
        return(list(datasource = ds))
    }
    stop("Unexpected ds, must be a CrunchDataset or a URL to a dataset or datasource")
}
