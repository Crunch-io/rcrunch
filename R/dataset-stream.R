#' Tools for working with streaming data
#'
#' Crunch allows you to stream data to a dataset. Streaming data is useful for
#' datasets which have frequent updates (see 
#' [the Crunch documentation](http://docs.crunch.io/#streaming-rows) for more)
#' information. 
#'
#' @param x a CrunchDataset
#' @return nothing?
#' @name streaming
#' @aliases streaming pendingMessages
#' @examples
#' \dontrun{
#' # need examples!
#' }
NULL


#' @rdname streaming
#' @export
setMethod("pendingMessages", "CrunchDataset", function (x) {
    stream_cat <- ShojiEntity(crGET(shojiURL(x, "fragments", "stream")))
    stream_cat$pending_messages
})

#' Stream data to a Crunch dataset
#' 
#' @param ds a CrunchDataset
#' @param data a dataframe with data to send as a stream, The given data values
#'  must be in the Crunch I/O format (for example, category ids instead of 
#'  names or numeric_values)
streamRows <- function (ds, data) {
    payload <- by(data, 1:nrow(data), function(row) toJSON(row) )
    payload <- paste0(payload, collapse = "\n") 
    out <- crPOST(shojiURL(ds, "fragments", "stream"), body=payload)
}

#' Append rows that have been streamed to a Crunch dataset
#'
#' Crunch allows you to stream data to a dataset. Streaming data is useful for
#' datasets which have frequent updates (see 
#' [the Crunch documentation](http://docs.crunch.io/#streaming-rows) for more)
#' information. 
#'
#' @param ds a CrunchDataset
#' @param ... Additional multitable attributes to set. Options include
#' `name` and `description`.
#' @return nothing?
#' @examples
#' \dontrun{
#' # need examples!
#' }
#' @export
appendStreamedRows <- function (ds, ...) {
    n_msg <- pendingMessages(ds)
    if (n_msg < 1) {
        message("There's no pending stream data to be appended.")
        return()
    }

    # stream must be after all other arguments in case a user tries to pass 
    # stream as an argument (like name)
    body <- wrapEntity(type = "ldjson", ..., stream = NULL)
    batches_url <- shojiURL(ds, "catalogs", "batches")
    return(crPOST(batches_url, body = toJSON(body)))
}