#' Get the pending streams for a dataset
#'
#' Crunch allows you to stream data to a dataset. Streaming data is useful for
#' datasets which have frequent updates (see 
#' [the Crunch documentation](http://docs.crunch.io/#streaming-rows) for more)
#' information. `pendingMessages()` retrieves the number of pending messages and `appendStreamedRows()` appends all pending streamed rows to the dataset.
#'
#' @param ds a CrunchDataset
#' @return number of pending messages in the stream for the dataset
#' @export
pendingStream <- function (ds) {
    stopifnot(is.dataset(ds))
    stream_cat <- ShojiEntity(crGET(shojiURL(ds, "fragments", "stream")))
    stream_cat$pending_messages
}

#' Stream data to a Crunch dataset
#' 
#' @param ds a CrunchDataset
#' @param data a dataframe with data to send as a stream, The given data values
#'  must be in the Crunch I/O format (for example, category ids instead of 
#'  names or numeric_values)
#' @keywords internal
streamRows <- function (ds, data) {
    if (nrow(data) > 1) {
        payload <- by(data, seq_len(nrow(data)), function (row) toJSON(row))
        payload <- paste0(payload, collapse = "\n") 
        crPOST(shojiURL(ds, "fragments", "stream"), body=payload)
    }
    invisible(refresh(ds))
}

#' Append data that has been streamed to a dataset
#'
#' Crunch allows you to stream data to a dataset. Streaming data is useful for
#' datasets which have frequent updates (see 
#' [the Crunch documentation](http://docs.crunch.io/#streaming-rows) for more)
#' information. Crunch automatically appends streamed data perioadically,
#' however if you would like to trigger appending pending streamed data to a 
#' dataset you can use `appendStream()`.
#'
#' @param ds a CrunchDataset
#' @param ... Additional batch attributes to set when appending pending stream 
#' data. Options include: `name` and `description`.
#' @return the dataset with pending stream data appended.
#' @export
appendStream <- function (ds, ...) {
    stopifnot(is.dataset(ds))
    if (pendingStream(ds) < 1) {
        message("There's no pending stream data to be appended.")
        return(ds)
    }

    # stream must be after all other arguments in case a user tries to pass 
    # stream as an argument (like name)
    # body <- wrapEntity(type = "ldjson", ..., stream = NULL)
    # batches_url <- shojiURL(ds, "catalogs", "batches")
    # crPOST(batches_url, body = toJSON(body))
    ds <- addBatch(ds, type = "ldjson", ..., stream = NULL)
    
    return(ds)
}