#' Create or get a data destination on the crunch server
#'
#' Data destinations are S3 buckets that the crunch server can send
#' data to.
#'
#' @param name A unique name to refer to this destination by
#' @param data A list, generally containing type="s3folder", bucket
#' with the bucket's name, and base for a prefix to use when exporting
#' data to.
#' @param ... Saved for future expansion
#'
#' @returns A ShojiObject data destination
#' @export
#'
#' @examples
#' destination <- cr_create_destination(
#'     "my-destination",
#'     data = list(type = "s3folder", bucket = "my-bucket", base = "lake")
#' )
#' destination <- cr_destination("my-destination")
cr_destination <- function(name) {
    catalog <- crunch:::getAPIRoot() |>
        crunch::shojiURL("catalogs", "datadestinations") |>
        paste0("?name=", utils::URLencode(name)) |>
        crunch::crGET() |>
        crunch:::ShojiCatalog()

    if (length(catalog) == 0) {
        cli::cli_abort("No datadestination found with name {.str {name}}")
    }

    crunch:::ShojiObject(body = catalog[[1]], self = names(catalog@index)[1])
}

#' @rdname cr_destination
#' @export
cr_create_destination <- function(name, data, ...) {
    catalog_url <- crunch:::getAPIRoot() |>
        crunch::shojiURL("catalogs", "datadestinations")

    body <- list(name = name, data = data, ...)
    body <- crunch:::wrapEntity(body = body) |>
        crunch::toJSON()

    url <- crunch::crPOST(catalog_url, body = body)
    object <- crunch::crGET(url)
    crunch:::ShojiObject(body = object$body, self = object$self)
}

cr_arrow_fs <- function(destination, ...) {
    if (!is.destination(destination)) cli::cli_abort("{.arg destination} must be a destination")
    if (destination@body$data$type != "s3folder") cli::cli_abort("Only destinations of type s3folder are supported")

    fs <- arrow::s3_bucket(destination@body$data$bucket, ...)
    fs$cd(destination@body$data$base)
}

# TODO: Formalize destination object
# (Should it be official rcrunch::Shoji object, and thus S4, or do we
# come up with a successor to rcrunch)
# Should it be one object type even though the subpath couldn't exist on server
# or should it be two (as it is now) where one matches server and the other
# includes the subfolder for a particular export

# lightweight S3 class to handle arrow::FileSystem + path to file
DestinationSubpath <- function(destination, path) {
    structure(list(destination = destination, path = path), class = "DestinationSubpath")
}

is.DestinationSubpath <- function(x) {
    inherits(x, "DestinationSubpath")
}

is.destination <- function(x) {
    inherits(x, "ShojiObject") && setequal(names(x@body), c("name", "data"))
}
