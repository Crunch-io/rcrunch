#' Tools for working with streaming data
#'
#' Crunch allows you to stream data to a dataset. Streaming data is useful for
#' datasets which have frequent updates (see 
#' [the Crunch documentation](http://docs.crunch.io/#streaming-rows) for more)
#' information. 
#'
#' @param x CrunchDataset
#' @param data a dataframe with data to send as a stream, The given data values
#'  must be in the Crunch I/O format (for example, category ids instead of 
#'  names or numeric_values)
#' @return nothing?
#' @name streaming
#' @aliases streaming getPendingMessages streamRows appendStreamedRows
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
#' @param ds CrunchDataset
#' @param messages the number of messages that are pending to be appended to 
#'  the dataset (default: NULL, which will append all pending messages)
#' @param ... Additional multitable attributes to set. Options include
#' `name` and `description`.
#' @return nothing?
#' @aliases streaming getPendingMessages streamRows appendStreamedRows
#' @examples
#' \dontrun{
#' # need examples!
#' }
#' @export
appendStreamedRows <- function (ds, messages = NULL, ...) {
    n_msg <- pendingMessages(ds)
    if (n_msg < 1) {
        message("There's pending stream data to be appended.")
        return()
    }
    body <- wrapEntity(stream = messages, type = "ldjson", ...)
    batches_url <- shojiURL(ds, "catalogs", "batches")
    return(crPOST(batches_url, body = toJSON(body)))
}