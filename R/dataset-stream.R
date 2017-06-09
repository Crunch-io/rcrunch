#' Tools for working with streaming data
#'
#' Crunch allows you to stream data to a dataset. Streaming data is useful for
#' datasets which have frequent updates (see 
#' [the Crunch documentation](http://docs.crunch.io/#streaming-rows) for more)
#' information. `pendingMessages()` retrieves the number of pending messages and `appendStreamedRows()` appends all pending streamed rows to the dataset.
#'
#' @param x a CrunchDataset
#' @param ... Additional batch attributes to set when appending pending stream 
#' data. Options include
#' `name` and `description`.
#' @return number of pending messages for `pendingMessages()` and the dataset 
#' with appended messages `appendStreamedRows()`
#' @name streaming
#' @aliases streaming pendingMessages appendStreamedRows
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
#' @keywords internal
streamRows <- function (ds, data) {
    payload <- by(data, 1:nrow(data), function(row) toJSON(row) )
    payload <- paste0(payload, collapse = "\n") 
    out <- crPOST(shojiURL(ds, "fragments", "stream"), body=payload)
}

#' @rdname streaming
#' @export
setMethod("appendStreamedRows", "CrunchDataset", function (x, ...) {
    n_msg <- pendingMessages(x)
    if (n_msg < 1) {
        message("There's no pending stream data to be appended.")
        return()
    }

    # stream must be after all other arguments in case a user tries to pass 
    # stream as an argument (like name)
    body <- wrapEntity(type = "ldjson", ..., stream = NULL)
    batches_url <- shojiURL(x, "catalogs", "batches")
    crPOST(batches_url, body = toJSON(body))
    
    return(refresh(x))
})