#' Download a tabulation from the Crunch Lake
#'
#' @param ds A `CrunchDataset` or URI for a crunch dataset
#' @param rows,columns A character vector of aliases, a Crunch Variables Catalog, Crunch Variable,
#' or list of these things for the tabulation to be calculated over.
#' @param file Path to a file to save the tabulation as
#' @param ... Other options passed to the API
#' @param format Format option passed to API (default is to guess based on filename)
#'
#' @returns Filepath to the downloaded tabulation data
#' @export
#'
#' @examples
#' \dontrun{
#' tab_file <- download_lake_tabulation(ds, "wave", c("age", "healthy_eater"), "test.csv")
#' tab <- readr::read_csv(tab_file)
#' }
download_lake_tabulation <- function(ds, rows, columns, file, ..., format = guess_tabulation_format(file)) {
    stopifnot(crunch::is.dataset(ds))
    stopifnot(is.character(file))

    datasource_tabulate_url <- crunch:::getAPIRoot() |>
        crunch::shojiURL("catalogs", "datasources") |>
        crunch::crGET() |>
        crunch:::ShojiObject() |>
        crunch::shojiURL("views", "tabulate")

    # TODO: Can we support datasources too? Datasources also identified by the URI, but have different key
    # in request (datasource instead of dataset) Maybe Lake team could distinguish since URI would be unique?
    if (crunch::is.dataset(ds)) ds <- crunch::self(ds)

    rows <- standardize_tabulation_dim(rows)
    columns <- standardize_tabulation_dim(columns)

    output <- list(type = "download") # Can this always be structured this way?

    body <- list(
        dataset = ds,
        tabulation = list(rows = I(rows), columns = I(columns)),
        format = format,
        output = output,
        ...
    ) |>
        crunch:::wrapView() |>
        crunch::toJSON()

    crunch::crPOST(
        datasource_tabulate_url,
        body = body,
        status.handlers = list(`202` = download_lake_tabulation_handler(file))
    )
}

standardize_tabulation_dim <- function(x) {
    if (crunch::is.variable(x)) {
        x <- crunch::alias(x)
    } else if (inherits(x, "VariablesCatalog")) {
        x <- crunch::aliases(x)
    } else if (is.list(x)) {
        x <- lapply(x, standardize_tabulation_dim) |> unlist()
    }

    if (!is.character(x)) {
        direction <- sys.call()[[2]]
        stop(direction, " must be character vector of aliases, a variables catalog, or 1 or more CrunchVariables in a list")
    }
    x
}


download_lake_tabulation_handler <- function(file) {
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
            # TODO: Does it get split into multiple files?
            crunch:::crDownload(final_poll_response$result[[1]], file)
        }
    }
}


guess_tabulation_format <- function(file) {
    if (grepl("\\.csv$", file)) return(list(file_type = "csv"))
    stop("Unable to guess format from filename, please specify according to API documentation")
}
